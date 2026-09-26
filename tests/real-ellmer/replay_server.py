#!/usr/bin/env python3
"""Deterministic OpenAI Chat Completions replay server for real-ellmer CI."""

import argparse
import json
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer


TRANSLATION = {
    "r_code": "x <- 1",
    "assumptions": ["none"],
    "confidence": 0.9,
}

# A valid program_translation_v1 payload for the registry-schema agent path.
# The contract's ad-hoc fixture schema keeps the legacy TRANSLATION shape; the
# request body says which one is being asked for (only the registry schema
# declares side_effects).
PROGRAM_TRANSLATION = {
    "r_code": "x <- 1",
    "summary": "translated unit",
    "parameters": [],
    "defaults": {},
    "reads": [],
    "writes": [],
    "side_effects": [],
    "helper_use": [],
    "discovered_dependencies": [],
    "suspected_dependencies": [],
    "affected_outputs": [],
    "uncertainty": [],
}


def contains_pair(value, key, expected):
    if isinstance(value, dict):
        if value.get(key) == expected:
            return True
        return any(contains_pair(item, key, expected) for item in value.values())
    if isinstance(value, list):
        return any(contains_pair(item, key, expected) for item in value)
    return False


def contains_key(value, key):
    if isinstance(value, dict):
        if key in value:
            return True
        return any(contains_key(item, key) for item in value.values())
    if isinstance(value, list):
        return any(contains_key(item, key) for item in value)
    return False


def structured_payload(body):
    if contains_key(body, "static_runnability") or "schema program_review_v1" in json.dumps(body):
        return {"verdict": "reviewed_no_material_finding", "static_runnability": "looks_runnable", "findings": [], "unresolved_dependencies": []}
    if contains_key(body, "ok"):
        return {"ok": True}
    if contains_key(body, "side_effects") or "schema program_translation_v1" in json.dumps(body):
        return PROGRAM_TRANSLATION
    return TRANSLATION


class ReplayHandler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def do_GET(self):  # noqa: N802 - stdlib handler API
        """Return an empty Bedrock inventory without leaving loopback."""
        if not self.path.startswith("/bedrock-control/foundation-models"):
            self.send_error(
                404, "expected /bedrock-control/foundation-models"
            )
            return
        with open(self.server.log_file, "a", encoding="utf-8") as stream:
            stream.write(json.dumps({"path": self.path, "body": {}}) + "\n")
        payload = json.dumps({"modelSummaries": []}).encode("utf-8")
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(payload)))
        self.send_header("Connection", "close")
        self.end_headers()
        self.wfile.write(payload)

    def do_POST(self):  # noqa: N802 - stdlib handler API
        is_chat_completions = self.path.endswith("/chat/completions")
        is_responses = self.path.endswith("/responses")
        is_anthropic_messages = self.path.endswith("/anthropic/v1/messages")
        is_gemini_generate = self.path.endswith(":generateContent")
        if not (
            is_chat_completions
            or is_responses
            or is_anthropic_messages
            or is_gemini_generate
        ):
            self.send_error(
                404,
                "expected /chat/completions, /responses, "
                "/anthropic/v1/messages, or :generateContent",
            )
            return
        length = int(self.headers.get("Content-Length", "0"))
        raw = self.rfile.read(length)
        try:
            body = json.loads(raw.decode("utf-8"))
        except (UnicodeDecodeError, json.JSONDecodeError):
            self.send_error(400, "request body must be JSON")
            return

        with open(self.server.log_file, "a", encoding="utf-8") as stream:
            stream.write(json.dumps({"path": self.path, "body": body}) + "\n")

        if body.get("model") == "offline-parallel-rejection-model" and "temperature" in body:
            payload = json.dumps({"error": {"message": "Unsupported parameter: temperature"}}).encode()
            self.send_response(400)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(payload)))
            self.send_header("Connection", "close")
            self.end_headers()
            self.wfile.write(payload)
            return

        if contains_pair(body, "effort", "sas2r-invalid-effort") or contains_pair(
            body, "reasoning_effort", "sas2r-invalid-effort"
        ):
            payload = json.dumps({"error": {"message": "Invalid value for reasoning_effort"}}).encode()
            self.send_response(400)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(payload)))
            self.end_headers()
            self.wfile.write(payload)
            return

        if body.get("model") in ("offline-timeout-model", "offline-parallel-model"):
            time.sleep(0.15)

        has_tools = bool(body.get("tools"))
        has_tool_result = (
            contains_pair(body, "role", "tool")
            or contains_pair(body, "type", "function_call_output")
        )
        if has_tools and not has_tool_result:
            response_kind = "tool"
        elif has_tools and has_tool_result:
            response_kind = "gathered"
        else:
            response_kind = "structured"

        native_deepseek = "native-deepseek" in body.get("model", "")
        if native_deepseek and has_tools:
            completed = sum(m.get("role") == "tool" for m in body.get("messages", []))
            response_kind = "tool" if completed < 2 else "gathered"

        if is_anthropic_messages:
            response = self.anthropic_messages_response(body)
        elif is_gemini_generate:
            response = self.gemini_generate_content_response(body, "native-gemini" in self.path)
        elif is_chat_completions:
            response = self.chat_completions_response(body, response_kind)
        else:
            response = self.responses_response(body, response_kind)
        payload = json.dumps(response).encode("utf-8")
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(payload)))
        self.send_header("Connection", "close")
        self.end_headers()
        try:
            self.wfile.write(payload)
        except BrokenPipeError:
            pass

    @staticmethod
    def chat_completions_response(body, response_kind):
        native = "native-deepseek" in body.get("model", "")
        batch = 1 + sum(m.get("role") == "tool" for m in body.get("messages", []))
        if response_kind == "tool":
            message = {
                "role": "assistant",
                "content": None,
                "tool_calls": [{
                    "id": "real_auto_1",
                    "type": "function",
                    "function": {
                        "name": "lookup_rulebook" if body.get("model", "").startswith("offline-parallel-") else "lookup",
                        "arguments": json.dumps({"name": "round"}),
                    },
                }],
            }
            if body.get("model") == "offline-batch-model":
                message["tool_calls"].append({
                    **message["tool_calls"][0], "id": "real_auto_2"
                })
            finish_reason = "tool_calls"
        elif response_kind == "gathered":
            message = {"role": "assistant", "content": "gathered context"}
            finish_reason = "stop"
        else:
            message = {
                "role": "assistant",
                "content": json.dumps(structured_payload(body), separators=(",", ":")),
            }
            finish_reason = "stop"
        if native:
            message["reasoning_content"] = f"native reasoning batch {batch}"
            if response_kind == "tool":
                message["tool_calls"][0]["id"] = f"native_{batch}"
        return {
            "id": "chatcmpl-sas2r-offline",
            "object": "chat.completion",
            "created": int(time.time()),
            "model": body.get("model", "gpt-4o-mini"),
            "choices": [{
                "index": 0,
                "message": message,
                "finish_reason": finish_reason,
            }],
            "usage": {
                "prompt_tokens": 100,
                "completion_tokens": 25,
                "total_tokens": 125,
            },
        }

    @staticmethod
    def responses_response(body, response_kind):
        if response_kind == "tool":
            # `id` and `call_id` carry the same identifier so the paired
            # request/result assertion does not depend on which of the two
            # fields a given ellmer release reads back.
            output = [{
                "id": "real_auto_1",
                "type": "function_call",
                "status": "completed",
                "call_id": "real_auto_1",
                "name": "lookup_rulebook" if body.get("model", "").startswith("offline-parallel-") else "lookup",
                "arguments": json.dumps({"name": "round"}),
            }]
            if body.get("model") == "offline-batch-model":
                output.append({**output[0], "id": "real_auto_2", "call_id": "real_auto_2"})
        else:
            text = "gathered context" if response_kind == "gathered" else json.dumps(
                structured_payload(body), separators=(",", ":")
            )
            output = [{
                "id": "msg_sas2r_offline",
                "type": "message",
                "status": "completed",
                "role": "assistant",
                "content": [{
                    "type": "output_text",
                    "text": text,
                    "annotations": [],
                }],
            }]
        return {
            "id": "resp_sas2r_offline",
            "object": "response",
            "created_at": int(time.time()),
            "status": "completed",
            "error": None,
            "incomplete_details": None,
            "model": body.get("model", "gpt-4o-mini"),
            "output": output,
            "parallel_tool_calls": True,
            "tool_choice": "auto",
            "tools": body.get("tools", []),
            "usage": {
                "input_tokens": 100,
                "input_tokens_details": {"cached_tokens": 0},
                "output_tokens": 25,
                "output_tokens_details": {"reasoning_tokens": 0},
                "total_tokens": 125,
            },
        }

    @staticmethod
    def anthropic_messages_response(body):
        """One native Anthropic Messages reply, not an OpenAI-shaped one."""
        return {
            "id": "msg_sas2r_offline",
            "type": "message",
            "role": "assistant",
            "model": body.get("model", "offline-anthropic-model"),
            "content": [{
                "type": "text",
                "text": json.dumps(TRANSLATION, separators=(",", ":")),
            }],
            "stop_reason": "end_turn",
            "stop_sequence": None,
            "usage": {"input_tokens": 100, "output_tokens": 25},
        }

    @staticmethod
    def gemini_generate_content_response(body, native=False):
        """One native Gemini generateContent reply."""
        parts = [{"text": json.dumps(TRANSLATION, separators=(",", ":"))}]
        if native:
            completed = sum("functionResponse" in part for turn in body.get("contents", [])
                            for part in turn.get("parts", []))
            if body.get("tools") and completed < 2:
                parts = [{"functionCall": {
                    "name": "lookup",
                    "args": {"name": "round"}},
                    "thoughtSignature": f"c2lnbmF0dXJl{completed + 1}"}]
                # The declaration identifies the real workflow's tool name.
                if contains_pair(body["tools"], "name", "lookup_rulebook"):
                    parts[0]["functionCall"]["name"] = "lookup_rulebook"
            elif body.get("tools"):
                parts = [{"text": "gathered context", "thoughtSignature": "Z2F0aGVyZWQ="}]
            else:
                parts = [{"text": json.dumps(structured_payload(body), separators=(",", ":"))}]
        return {
            "candidates": [{
                "content": {
                    "role": "model",
                    "parts": parts,
                },
                "finishReason": "STOP",
                "index": 0,
            }],
            "usageMetadata": {
                "promptTokenCount": 100,
                "candidatesTokenCount": 25,
                "totalTokenCount": 125,
            },
            "modelVersion": "offline-gemini-model",
        }

    def log_message(self, *_args):
        return


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--port-file", required=True)
    parser.add_argument("--log-file", required=True)
    args = parser.parse_args()

    server = ThreadingHTTPServer(("127.0.0.1", 0), ReplayHandler)
    server.log_file = args.log_file
    with open(args.port_file, "w", encoding="utf-8") as stream:
        stream.write(str(server.server_address[1]))
    server.serve_forever()


if __name__ == "__main__":
    main()
