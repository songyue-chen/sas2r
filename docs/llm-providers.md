# LLM Provider Configuration & Authentication Guide

`sas2r` provides a closed registry of twelve native LLM providers powered by the `ellmer` package. All LLM calls route through named public `ellmer` constructor functions. `sas2r` does not implement direct provider HTTP APIs and does not support a generic `openai_compatible` provider route in this release.

---

## 1. Closed Provider Registry

The twelve supported provider IDs and their dispatch contracts:

| Provider ID | ellmer Constructor | Auth Modes (`default`) | Required Selectors | Optional Selectors | Credential Environment Variables / Ambient Sources | Model Inventory (`sas_llm_models`) | Acceptance Level |
|---|---|---|---|---|---|---|---|
| `openai` | `ellmer::chat_openai` | `ambient`, `api_key` (`api_key`) | *(none)* | `base_url`, `credentials`, `api_key` | `OPENAI_API_KEY` (resolved by `ellmer`, never read by `sas2r`) | Available (`models_openai`) | Offline contract |
| `anthropic` | `ellmer::chat_anthropic` | `api_key` (`api_key`) | *(none)* | `base_url`, `credentials`, `api_key`, `cache` (`5m`/`1h`/`none`) | `ANTHROPIC_API_KEY` | Available (`models_anthropic`) | Offline contract |
| `bedrock` | `ellmer::chat_aws_bedrock` | `ambient` (`ambient`) | `region` **xor** `base_url` | `profile`, `cache` (`auto`/`5m`/`1h`/`none`) | `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_SESSION_TOKEN`; AWS SSO / IAM Identity Center / CLI credential chain (`aws sso login --profile <profile>`) | Available only with an explicit `region` and a non-inference-profile model; `inventory_unavailable` for inference profiles or a custom `base_url` | Offline contract + opt-in live smoke |
| `azure` | `ellmer::chat_azure_openai` | `ambient`, `api_key` (`ambient`) | `endpoint`, `api_version`; plus `api_key` when `auth_mode: api_key` | `credentials` | `AZURE_OPENAI_API_KEY`, `AZURE_CLIENT_SECRET`; Azure CLI / Entra ID / Managed Identity (`az login`) | Unavailable (no `models_*` export; deployment-based) | Offline contract + opt-in live smoke |
| `databricks` | `ellmer::chat_databricks` | `ambient` (`ambient`) | *(none)* | `workspace` | `DATABRICKS_TOKEN`; Databricks CLI profile, Workbench, or Connect (`databricks auth login --host <workspace>`). `DATABRICKS_HOST` is read for tenant identity only and is not a credential | Unavailable (no `models_*` export) | Offline contract |
| `deepseek` | `ellmer::chat_deepseek` | `api_key` (`api_key`) | *(none)* | `base_url`, `credentials`, `api_key` | `DEEPSEEK_API_KEY` | Available (`models_deepseek`) | Offline contract |
| `github` | `ellmer::chat_github` | `api_key` (`api_key`) | *(none)* | `base_url`, `models_base_url`, `credentials`, `api_key` | `GITHUB_PAT` | Available by default; a custom chat `base_url` makes it `inventory_unavailable` unless `models_base_url` is also set | Offline contract |
| `gemini` | `ellmer::chat_google_gemini` | `ambient`, `api_key` (`ambient`) | Under `auth_mode: api_key`, one of `api_key`, `credentials`, or `GOOGLE_API_KEY`/`GEMINI_API_KEY` present in the environment | `base_url`, `credentials`, `api_key` | `GOOGLE_API_KEY`, `GEMINI_API_KEY`, or Google Application Default Credentials (`gcloud auth application-default login`) | Available (`models_google_gemini`) | Offline contract |
| `vertex` | `ellmer::chat_google_vertex` | `ambient` (`ambient`) | `project_id`, `location` | *(none — `credentials` is declared but always rejected; Vertex uses ADC)* | `GOOGLE_APPLICATION_CREDENTIALS`; Google Application Default Credentials (`gcloud auth application-default login`) | Available (`models_google_vertex`) | Offline contract + opt-in live smoke |
| `ollama` | `ellmer::chat_ollama` | `none` (`none`) | `base_url` | *(none)* | None consumed. `OLLAMA_API_KEY` is on the redaction allowlist only | Available (`models_ollama`) | Offline argument-shape only |
| `posit` | `ellmer::chat_posit` | `ambient` (`ambient`) | *(none)* | `base_url`, `credentials`, `cache` (`5m`/`1h`/`none`) | None documented. `ellmer` owns the Posit OAuth / device sign-in cache | Available (`models_posit`) | Offline contract |
| `snowflake` | `ellmer::chat_snowflake` | `ambient` (`ambient`) | *(none)* | `account`, `credentials` | `SNOWFLAKE_TOKEN`, `SNOWFLAKE_PRIVATE_KEY`; Workbench or Connect viewer token. `SNOWFLAKE_ACCOUNT` and `SNOWFLAKE_USER` are identity selectors, not credentials | Unavailable (no `models_*` export) | Offline contract |

Every configuration must also name an explicit `model` or a `tiers` mapping; `sas2r` never inherits a library default.

### What the acceptance levels mean

The three levels are not interchangeable. Read them as evidence, not as endorsement.

- **Offline argument-shape only** — the arguments `sas2r` builds for the provider are checked against the real `ellmer` constructor's published formals, and both exports are confirmed to be in `ellmer`'s namespace. No constructor call against real `ellmer` is made. Only `ollama` sits here; its constructor is exercised solely against the committed `ellmer` S7 test stub.
- **Offline contract** — everything above, plus the real `ellmer` constructor is actually invoked with registry-built arguments in `tests/real-ellmer/contract.R`, against a loopback replay server on `127.0.0.1`. Credentialed paths are stopped at an explicit offline boundary before any request is built. This job runs unconditionally in CI on every push and pull request, on both the pinned and current `ellmer`, and consumes no secrets.
- **Opt-in live smoke** — additionally has a credentialed probe script under `inst/smoke/`, gated behind `SAS2R_SMOKE_BEDROCK`, `SAS2R_SMOKE_AZURE`, or `SAS2R_SMOKE_VERTEX` set to `true` plus that provider's endpoint variables. These scripts are never run by CI; the test suite asserts only that they refuse to run without the gate. The other nine providers have no live path in this repository.

No test in this package contacts a provider, starts an OAuth flow, invokes a CLI, or reads a developer credential file.

> [!IMPORTANT]
> `provider: openai_compatible` and arbitrary constructor names are rejected. Configuration must name one of the twelve validated provider IDs listed above.

---

## 2. Configuration Examples (`_sas2r.yml`)

Never commit literal API keys, tokens, or private secrets into `_sas2r.yml`. Always supply secrets via shell environment variables. Every configuration should specify an explicit `model` or `tiers` definition rather than relying on changing library defaults.

### OpenAI
```bash
export OPENAI_API_KEY="sk-..."
```
```yaml
llm:
  provider: openai
  auth_mode: api_key
  model: gpt-4o
  tiers:
    frontier: gpt-4o
    cheap: gpt-4o-mini
```

### Anthropic
```bash
export ANTHROPIC_API_KEY="sk-ant-..."
```
```yaml
llm:
  provider: anthropic
  auth_mode: api_key
  model: claude-3-5-sonnet-20241022
  cache: 1h                             # 5m | 1h | none (default: 1h -- migration turns outlive a 5m TTL)
  tiers:
    frontier: claude-3-5-sonnet-20241022
    cheap: claude-3-5-haiku-20241022
```

### AWS Bedrock
Authenticate via AWS CLI / SSO prior to launching R (`aws sso login --profile clinical-dev`):
```yaml
llm:
  provider: bedrock
  auth_mode: ambient
  profile: clinical-dev
  region: us-east-1
  model: us.anthropic.claude-3-5-sonnet-20241022-v2:0
  cache: auto                           # auto | 5m | 1h | none
```

### Azure OpenAI
Authenticate via Azure CLI (`az login`) or set `auth_mode: api_key`:
```yaml
llm:
  provider: azure
  auth_mode: ambient
  endpoint: https://my-resource.openai.azure.com
  api_version: 2024-10-21
  model: my-gpt4o-deployment
```

### Databricks
Authenticate via Databricks CLI or environment variables (`DATABRICKS_HOST`, `DATABRICKS_TOKEN`):
```yaml
llm:
  provider: databricks
  auth_mode: ambient
  workspace: https://my-org.cloud.databricks.com
  model: databricks-meta-llama-3-3-70b-instruct
```

### DeepSeek
```bash
export DEEPSEEK_API_KEY="sk-..."
```
```yaml
llm:
  provider: deepseek
  auth_mode: api_key
  model: deepseek-chat
  capabilities:
    tool_calling: native      # see section 5; without it the agent layer is skipped
  timeout_seconds: 900
```

DeepSeek ships `structured_output: fallback` deliberately. Do not override it
to `native` -- the provider answers HTTP 400.

### GitHub Models
```bash
export GITHUB_PAT="ghp_..."
```
```yaml
llm:
  provider: github
  auth_mode: api_key
  model: gpt-4o
```

### Google Gemini API
```bash
export GEMINI_API_KEY="..." # or GOOGLE_API_KEY
```
```yaml
llm:
  provider: gemini
  auth_mode: api_key
  model: gemini-1.5-pro
```

### Google Vertex AI
Authenticate via ADC (`gcloud auth application-default login`):
```yaml
llm:
  provider: vertex
  auth_mode: ambient
  project_id: my-gcp-project
  location: us-central1
  model: gemini-1.5-pro
```

### Ollama (Local)
```yaml
llm:
  provider: ollama
  auth_mode: none
  base_url: http://localhost:11434
  model: llama3.1:8b
```

### Posit AI
Authenticate via Posit Workbench/Connect OAuth:
```yaml
llm:
  provider: posit
  auth_mode: ambient
  model: claude-3-5-sonnet
```

### Snowflake Cortex
Authenticate via Snowflake ambient credentials:
```yaml
llm:
  provider: snowflake
  auth_mode: ambient
  account: my-org-account
  model: llama3.1-70b
```

---

## 3. Pre-Flight Verification (`sas_llm_models` and `sas_llm_probe`)

Validate your provider configuration and connectivity before initiating translations:

```r
cfg <- sas2r::sas_config("_sas2r.yml")

# 1. Discover available models (read-only, when provider supports inventory)
models <- sas2r::sas_llm_models(cfg)
print(models)

# 2. Probe connection and structured-output support (minimal request).
llm <- sas2r::sas_llm(cfg)
probe <- sas2r::sas_llm_probe(llm, tier = "frontier")
print(probe)
```

`sas_llm()` is the supported constructor for the adapter every agent phase and
`sas_refine_with_outputs()` take as their `llm` argument. It reads the same
`llm:` mapping shown above, contacts no network, and leaves credentials with
`ellmer`.

### Inventory vs. Probe Semantics
- **`sas_llm_models()`**: Queries the provider inventory endpoint where supported. An `inventory_unavailable` status indicates the provider does not expose an inventory endpoint (e.g. Azure, Databricks, Snowflake) or uses custom inference profiles; it does **not** indicate an empty model list.
- **`sas_llm_probe()`**: Tests authentication, endpoint reachability, and structured-output support on the configured model with a minimal 32-token request. The ping carries no tools, so it does **not** exercise tool calling; a model that answers the probe may still lack tool support, which surfaces at the first tool-using phase. The attempt is ledgered only when you pass a `usage_budget` carrying a `ledger_path`, as a translation run does through its shared budget. Probe never launches interactive browser logins in automated CI.

---

## 4. Usage Ledger & Budget Policy

Every LLM attempt (translation, review, repair, tool calls, and probes) reserves policy limits immediately before transport and appends audit records to `<out_dir>/.sas2r/usage.jsonl`, where `<out_dir>` is the translation's output directory.

### Default Observe Mode
Without an explicit `budget:` block in `_sas2r.yml`, the default policy is:
```yaml
budget:
  mode: observe
  max_usd: Inf
```
In `observe` mode, requests execute without a dollar limit while all token dimensions, timestamps, and cost provenances are logged.

### Optional Enforceable Limits
Every ceiling defaults to `Inf`. An unset ceiling is not a safe default -- it is
no ceiling at all:
```yaml
budget:
  mode: soft                            # observe | soft | strict
  max_usd: 10.00
  max_calls: 50                         # total requests
  max_retries: 2                        # sas2r-level retries (see note below)
  max_tool_calls: 500                   # total tool executions across the run
  max_wall_time: 7200                   # seconds, whole run
  max_output_tokens: 128000             # keep well clear of the model's own
                                        # maximum; a low ceiling truncates
                                        # reasoning models mid-answer
  max_request_bytes: 1048576
  max_request_chars: 500000
  max_input_tokens: 128000
```
- **`mode: soft`**: Halts subsequent requests once cumulative recorded spend reaches `max_usd`.
- **`mode: strict`**: Enforces strict upfront output token reservations against locked organization rate cards.

> **`max_usd` binds only when the provider reports cost.** Where pricing is
> unavailable, every record carries `cost_status: unknown`, cumulative spend
> stays `0`, and the dollar ceiling never trips. `sas2r` warns when this
> happens. Bound such runs with the non-dollar ceilings above.

> **Two retry layers exist.** `budget: max_retries:` counts sas2r-level retries
> only. Beneath it, ellmer retries each HTTP request `ellmer_max_tries` times
> (default 3), which the ledger does not see -- so a run can issue three times
> the requests it appears to. Set `llm: max_tries:` to bound that layer.

### Cost Provenance
`sas2r` records cost under five provenance states: `billed_amount`, `contract_estimate`, `catalog_estimate`, `incomplete_estimate`, or `unknown`. `sas2r` owns no built-in fallback price table; unknown pricing remains `unknown` and is never estimated from arbitrary hard-coded rates.

---

## 5. Model Capabilities & Fallbacks

### Declaring what your endpoint supports (`capabilities:`)

`sas2r` never assumes a capability it has not been told about. Each capability
resolves to `supported`, `unsupported`, or `unknown`, and **the runner fails
closed on `unknown`**. No provider in the registry ships a `tool_calling`
default other than `unknown`, so unless you declare it, every agent unit is
skipped with a `tool_calling_unavailable` flag and the run still exits
successfully:

```yaml
llm:
  provider: deepseek
  model: deepseek-v4-flash
  capabilities:
    tool_calling: native        # required to run the agent layer at all
```

Declare only what your endpoint genuinely supports. `structured_output` in
particular is provider-specific: DeepSeek ships `fallback` deliberately, and
forcing `native` there makes the provider reject the request with HTTP 400.

### Optional parameters are withheld unless confirmed

Optional parameters such as `temperature` and `reasoning_effort` are sent only
when their capability is exactly `supported`. Requesting `temperature: 0`
against a provider whose capability is `unknown` does not error -- the request
is answered at the provider default. Every such omission is named in the
`withheld_parameters` field of the audit record, so what was actually sent is
always recoverable from the log.

An optional parameter can be set per project, and an agent spec overrides it
where the spec speaks -- the shipped translator sets `temperature: 0` for
determinism, and a project default does not undo that. No shipped spec sets
`reasoning_effort`, so in practice it comes entirely from configuration:

```yaml
llm:
  reasoning_effort: high
  capabilities:
    reasoning_effort: supported     # required; the gate fails closed on unknown
```

> **Delivery depends on ellmer.** `sas2r` hands optional parameters to ellmer,
> which maps only those it supports for a given provider and drops the rest
> with an `Ignoring unsupported parameters` warning. As of ellmer 0.4.2,
> `reasoning_effort` is not mapped for `chat_deepseek()`, so declaring it has
> no effect there and ellmer warns. `sas2r` does not work around this: the
> parameter starts working when ellmer adds support, with no change here.

> **Reasoning tokens are not counted.** ellmer's public token surface reports
> `input`, `output`, and `cached_input` only. Where a provider bills reasoning
> tokens separately, they are absent from the ledger and from cost estimates.

### Request timeout and retries (`timeout_seconds`, `max_tries`)

Each HTTP request is bounded by ellmer's `ellmer_timeout_s` option, defaulting
to 300 seconds, and retried `ellmer_max_tries` times, defaulting to 3. A
frontier model answering through a chain of tool calls can exceed the timeout
and fail mid-stream with `sas2r_llm_timeout` -- three times over, so a single
doomed unit can consume 15 minutes before reporting failure.

```yaml
llm:
  timeout_seconds: 900     # per HTTP request, not per run
  max_tries: 2             # ellmer-level attempts per request
```

`timeout_seconds` applies to one HTTP request; bound total runtime with
`budget: max_wall_time:`. `max_tries` is the ellmer-level companion to
`budget: max_retries:` -- set both, or the layer you did not set stays
unbounded.

### Other behaviour

- If a provider rejects structured JSON output natively, `sas2r` seamlessly falls back to schema-guided repair prompts.
- All credential values, tokens, and authorization headers are redacted from console output, logs, and `_sas2r.lock`.

---

## 5. Data & Model Privacy Boundary

`sas2r` maintains a declared, bounded boundary between local data and remote language models:

- **Default Payload**: Program source code, macro interfaces, structural column metadata, and redacted comparison digests (variable names, mismatch counts, difference magnitudes, pattern hints — never the mismatching cells) are what cross the model boundary.
- **Bounded Opt-In Surfaces**: The reviewer's bounded comparison report may quote a small, capped set of example differences — including row numbers, key values, and differing cell values (subject identifiers among them when key columns identify subjects) — and `agent_evidence = "bounded"` adds capped output metadata with short previews to repair evidence. The default `agent_evidence = "code_only"` sends neither.
- **Data Residency**: All dataset reading, writing, and execution take place in the local R process on your infrastructure; confirm the endpoint you configure meets your enterprise data residency obligations.
