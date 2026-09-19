# LLM Provider Configuration & Authentication Guide

`sas2r` provides a closed registry of twelve native LLM providers powered by the `ellmer` package. All LLM calls route through named public `ellmer` constructor functions. `sas2r` does not implement direct provider HTTP APIs and does not support a generic `openai_compatible` provider route in this release.

AI translation requires **ellmer 0.4.2 or newer**; **ellmer 0.5.0 is recommended
for new installations and required for parallel translation**. Older ellmer
installations visibly use one workflow. Both versions have offline compatibility
tests; their request-counting units differ as described below.
Install or update it with `install.packages("ellmer")`, then restart R.
References to 0.4.2 below describe the minimum supported connector baseline,
not a requirement to install that exact version. Startup verification checks
explicit settings against your installed connector and selected model.

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
| `github` | `ellmer::chat_github` | `api_key` (`api_key`) | *(none)* | `base_url`, `models_base_url`, `credentials`, `api_key` | `GITHUB_PAT` | **Retired upstream.** GitHub Models was retired on 2026-07-30 and `ellmer` >= 0.5.0 makes `chat_github()`/`models_github()` defunct; `sas_llm()` and `sas_llm_models()` refuse the provider with `sas2r_llm_provider_retired` unless `ellmer` < 0.5.0 is installed (there: available by default; a custom chat `base_url` makes it `inventory_unavailable` unless `models_base_url` is also set) | Offline contract (ellmer 0.4.2); retirement refusal verified on current ellmer |
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

**Evaluate Gemini Flash or DeepSeek Flash first**, retaining reasoning and all
translation/review/repair checks. Frontier models remain available for programs
where the first model leaves material source-based findings. This is a practical
starting strategy, not a guarantee that any model is sufficient for every study.
The complete provider profiles below are the shared reference for the README
and migration guides.
Copy one complete `llm:` block; the model must be available to your account.

### Recommended starting settings

These are suggested YAML profiles, not automatic provider-specific package
defaults. Settings/model documentation checked **September 18, 2026**. Startup
verifies explicit parameters against the installed connector and selected model.

| Provider / model | Reasoning | `llm.max_output_tokens` | `llm.timeout_seconds` | Structured output |
| --- | --- | ---: | ---: | --- |
| Gemini / `gemini-3.8-flash` | `high` | 65536 | 900 | `fallback` |
| DeepSeek / `deepseek-flash` | Keep server thinking default; omit explicit effort on the documented ellmer route | 131072 | 1800 | `fallback` |
| OpenAI / `gpt-5.6-terra` | `high` | 32768 | 900 | `native` |
| Anthropic / `claude-sonnet-4-6` | `high` with adaptive thinking | 32768 | 900 | `fallback`; `cache: 1h` |

Use `tool_calling: native` and `max_tries: 1` with all four profiles. Leave
`temperature` and `top_p` unset. Output ceilings include reasoning where the
provider counts it; they are not fixed consumption targets. Longer allowances
may cost more and take longer. The package's unset timeout default remains 300
seconds; the profiles give reasoning requests more time. A timeout is per HTTP
attempt, not an inactivity timer or a whole-translation deadline.

For Vertex with the same Gemini model, start from the Gemini settings and supply
your project/location. Posit's Claude route can use the Claude settings. For
Azure, Bedrock, Databricks and Snowflake, use the actual deployment's supported
output limit and default reasoning; do not copy an explicit effort setting that
the connector cannot forward. Start Ollama at one concurrent translation and
choose a reasoning/tool-capable model that fits local memory. GitHub Models is
retired and is not recommended for new configurations.

For **every provider**, establish a checked baseline at:

```yaml
migration:
  max_parallel_translations: 1
```

Then evaluate `2` concurrent program-or-macro workflows on the same inputs and
settings. Raise to `3` or `4` only when quality checks, endpoint quotas and memory
permit. Provider limits can depend on model, account, region and deployment;
there is no universal safe provider concurrency number. This setting counts whole
translation workflows, not translator/reviewer/fixer roles. Local execution and
repair remain serial. No CPU-count clamp is applied.

The adapter retains native conversation history, including DeepSeek reasoning
content and Gemini thought signatures, through tool gathering and finalization.
Offline replay tests cover multiple tool batches and concurrent workers on
ellmer 0.5.0; ellmer 0.4.2 retains its serial native loop. These tests establish
transport behavior, not study-level translation quality or live speedup.

On ellmer 0.5.0+, `usage_limits.max_calls` counts each request admitted within a
tool conversation and each finalization request, in either mode. This can reach
an old call ceiling sooner than ellmer 0.4.2, which retains legacy phase-level
metering in serial mode. Use `max_tries: 1` for individually accounted requests;
connector-internal retries with a larger value are not separate admissions.
If both `max_parallel_translations` and `max_tries` exceed 1, preflight and
translation stop before provider calls with instructions to set either value to 1.
The existing agent retry policy remains separately metered. See the
[usage evidence guide](migration-evidence.md#coverage-limits-and-reuse).

The `frontier` tier name in agent routing can map to a Flash model. It is a
configuration label, not an instruction to buy a particular model class.

### Adjust settings according to the result

- **Incomplete output:** inspect the finish reason and effective output allowance.
  Increase a too-small allowance within the model limit; also check context size.
  A summary covering multiple tool turns is not the size of one response. Do not
  accept partial code or lower reasoning automatically to obtain a completion.
- **Timeout:** allow more time for an otherwise valid long request, and check
  endpoint latency. Increasing retries can multiply both elapsed time and spend.
- **Rate limit / overload:** reduce concurrent translations and check the
  endpoint's request/token quotas. More parallel work can make throttling worse.
- **Unsupported/ignored settings:** follow the connector-specific profile and
  startup verification. A capability flag does not make a connector forward a
  parameter it cannot carry. Do not disable verification to force a request.
- **Tool/history errors:** confirm the installed connector can preserve the
  provider's required multi-turn state. Increasing timeout or tokens cannot fix
  a protocol mismatch. A tool-free connection probe does not verify this path.
- **Completed but incorrect output:** inspect source-based review findings and
  complete output values/metadata; consider a stronger model with the same checks.
  A stronger model does not replace a missing input or unsupported runtime feature.

`llm.max_output_tokens` sets the requested per-response allowance.
`budget.max_output_tokens` is an admission ceiling for that request; when both
are set, keep the budget ceiling at least as large as the requested allowance.
Use run-level call, tool, time and dollar limits separately. An unknown monetary
cost is not zero cost or proof that a dollar ceiling can be enforced.

Sources: [Gemini model/settings](https://ai.google.dev/gemini-api/docs/latest-model),
[Gemini quotas](https://ai.google.dev/gemini-api/docs/rate-limits),
[DeepSeek request parameters](https://api-docs.deepseek.com/api/create-chat-completion/),
[DeepSeek thinking](https://api-docs.deepseek.com/guides/thinking_mode/),
[OpenAI Terra](https://developers.openai.com/api/docs/models/gpt-5.6-terra), and
[Claude thinking](https://platform.claude.com/docs/en/build-with-claude/thinking-steering-and-cost).
Provider facts describe API capabilities; the exact ceilings/timeouts above are
sas2r starting recommendations, subject to connector and account verification.

Never commit literal API keys, tokens, or private secrets into `_sas2r.yml`. Always supply secrets via shell environment variables. Every configuration should specify an explicit `model` or `tiers` definition rather than relying on changing library defaults.

### OpenAI
```bash
export OPENAI_API_KEY="sk-..."
```
```yaml
llm:
  provider: openai
  auth_mode: api_key
  model: gpt-5.6-terra
  reasoning_effort: high
  max_output_tokens: 32768
  capabilities:
    structured_output: native
    tool_calling: native
  timeout_seconds: 900
  max_tries: 1
```

### Anthropic
```bash
export ANTHROPIC_API_KEY="sk-ant-..."
```
```yaml
llm:
  provider: anthropic
  auth_mode: api_key
  model: claude-sonnet-4-6
  reasoning_effort: high
  max_output_tokens: 32768
  capabilities:
    structured_output: fallback
    tool_calling: native
  cache: 1h
  timeout_seconds: 900
  max_tries: 1
```

### AWS Bedrock
Authenticate via AWS CLI / SSO prior to launching R (`aws sso login --profile clinical-dev`):
```yaml
llm:
  provider: bedrock
  auth_mode: ambient
  profile: clinical-dev
  region: us-east-1
  model: us.anthropic.claude-sonnet-4-6-v1:0
  cache: auto
  # ellmer 0.4.2 does not forward effort on this route; model defaults apply.
  # If explicitly requested high reasoning is required, use a supported route.
  max_output_tokens: 32768           # requires this allowance on your endpoint
  capabilities:
    structured_output: fallback
    tool_calling: native
    reasoning_effort: unsupported
  timeout_seconds: 900
  max_tries: 1
```

### Azure OpenAI
Authenticate via Azure CLI (`az login`) or set `auth_mode: api_key`:
```yaml
llm:
  provider: azure
  auth_mode: ambient
  endpoint: https://my-resource.openai.azure.com
  api_version: 2024-10-21
  model: my-reasoning-model-deployment
  # ellmer 0.4.2 does not forward effort on this route; model defaults apply.
  # If explicitly requested high reasoning is required, use a supported route.
  max_output_tokens: 32768           # requires this allowance on your endpoint
  capabilities:
    structured_output: fallback
    tool_calling: native
    reasoning_effort: unsupported
  timeout_seconds: 900
  max_tries: 1
```

### Databricks
Authenticate via Databricks CLI or environment variables (`DATABRICKS_HOST`, `DATABRICKS_TOKEN`):
```yaml
llm:
  provider: databricks
  auth_mode: ambient
  workspace: https://my-org.cloud.databricks.com
  model: databricks-claude-sonnet-4-6
  # ellmer 0.4.2 does not forward effort on this route; model defaults apply.
  # If explicitly requested high reasoning is required, use a supported route.
  max_output_tokens: 32768           # requires this allowance on your endpoint
  capabilities:
    structured_output: fallback
    tool_calling: native
    reasoning_effort: unsupported
  timeout_seconds: 900
  max_tries: 1
```

### DeepSeek
```bash
export DEEPSEEK_API_KEY="sk-..."
```
```yaml
llm:
  provider: deepseek
  auth_mode: api_key
  model: deepseek-flash              # or deepseek-v4-pro
  # DeepSeek currently defaults to thinking enabled, high effort.
  # ellmer 0.4.2 does not forward reasoning_effort on this route.
  max_output_tokens: 131072
  capabilities:
    structured_output: fallback
    tool_calling: native
    reasoning_effort: unsupported    # connector limitation; thinking is not disabled
  timeout_seconds: 1800
  max_tries: 1
```

DeepSeek uses `structured_output: fallback` on this sas2r/ellmer Chat Completions
route. Native-schema support on another DeepSeek API is not evidence that this
route accepts sas2r's structured-output request.

DeepSeek currently defaults to thinking enabled with high effort; the profile
relies on that [documented server default](https://api-docs.deepseek.com/guides/thinking_mode/).
ellmer 0.4.2 drops an explicit `reasoning_effort` on this route, so do not mark
that parameter supported to try to enable it. The `unsupported` declaration
above describes the connector and does not disable the model's thinking.

Model names checked September 12, 2026: `deepseek-flash` serves
DeepSeek-V4.1-Flash. `deepseek-v4-flash` and `deepseek-v4-flash-vision-exp` are
temporary aliases for it; `deepseek-v4-pro` remains available. The older
`deepseek-chat` and `deepseek-reasoner` names were scheduled for discontinuation
on July 24, 2026 and should not be used in new configurations. Use the current
[model list](https://api-docs.deepseek.com/quick_start/pricing/) and
[developer changelog](https://api-docs.deepseek.com/updates/) when updating a study.
The provider registry passes the configured model name to ellmer, so a new model
name does not require a sas2r code change.

### GitHub Models
> **Retired upstream.** GitHub Models was retired on 2026-07-30, and `ellmer`
> 0.5.0 made `chat_github()` and `models_github()` defunct. This provider only
> works with `ellmer` 0.4.2-0.4.x; on newer `ellmer`, `sas_llm()` refuses it
> with a `sas2r_llm_provider_retired` error before any request is built.
```bash
export GITHUB_PAT="ghp_..."
```
```yaml
llm:
  provider: github
  auth_mode: api_key
  model: gpt-4o                  # historical connection example, not a reasoning profile
  capabilities:
    structured_output: native
    tool_calling: native
```

### Google Gemini API
```bash
export GEMINI_API_KEY="..." # or GOOGLE_API_KEY
```
```yaml
llm:
  provider: gemini
  auth_mode: api_key
  model: gemini-3.8-flash
  reasoning_effort: high
  max_output_tokens: 65536
  capabilities:
    structured_output: fallback
    tool_calling: native
  timeout_seconds: 900
  max_tries: 1
```

### Google Vertex AI
Authenticate via ADC (`gcloud auth application-default login`):
```yaml
llm:
  provider: vertex
  auth_mode: ambient
  project_id: my-gcp-project
  location: us-central1
  model: gemini-3.8-flash
  reasoning_effort: high
  max_output_tokens: 65536
  capabilities:
    structured_output: fallback
    tool_calling: native
  timeout_seconds: 900
  max_tries: 1
```

### Ollama (Local)
This is a connection example, not a recommended reasoning profile. The shown
Llama model does not establish support for configurable thinking. Ollama's
reasoning and output-limit behavior depends on the served model and connector;
verify both before declaring their capabilities. Do not copy the cloud effort
flags onto an arbitrary local model.
```yaml
llm:
  provider: ollama
  auth_mode: none
  base_url: http://localhost:11434
  model: llama3.1:8b
  capabilities:
    structured_output: fallback
    tool_calling: native
```

### Posit AI
Authenticate via Posit Workbench/Connect OAuth:
```yaml
llm:
  provider: posit
  auth_mode: ambient
  model: claude-sonnet-4-6           # selects ellmer's Anthropic route
  reasoning_effort: high
  max_output_tokens: 32768
  capabilities:
    structured_output: fallback
    tool_calling: native
  cache: 1h
  timeout_seconds: 900
  max_tries: 1
```

### Snowflake Cortex
Authenticate via Snowflake ambient credentials:
```yaml
llm:
  provider: snowflake
  auth_mode: ambient
  account: my-org-account
  model: claude-sonnet-4-6
  # ellmer 0.4.2 does not forward effort on this route; model defaults apply.
  # If explicitly requested high reasoning is required, use a supported route.
  max_output_tokens: 32768           # requires this allowance on your endpoint
  capabilities:
    structured_output: fallback
    tool_calling: native
    reasoning_effort: unsupported
  timeout_seconds: 900
  max_tries: 1
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

`sas_llm()` is the supported constructor for the adapter passed to
`sas_translate()` as its `llm` argument. It reads the same
`llm:` mapping shown above, contacts no network, and leaves credentials with
`ellmer`.

### Inventory vs. Probe Semantics
- **`sas_llm_models()`**: Queries the provider inventory endpoint where supported. An `inventory_unavailable` status indicates the provider does not expose an inventory endpoint (e.g. Azure, Databricks, Snowflake) or uses custom inference profiles; it does **not** indicate an empty model list.
- **`sas_llm_probe()`**: Tests authentication, endpoint reachability, structured output, and forwarding of explicitly configured parameters. It uses the configured output ceiling, or 2048 tokens when effort is configured, otherwise 32. A standalone ping does not perform the startup negative control or populate its cache. The ping carries no tools, so it does **not** exercise tool calling; a model that answers the probe may still lack tool support, which surfaces at the first tool-using phase. The attempt is ledgered only when you pass a `usage_budget` carrying a `ledger_path`, as a translation run does through its shared budget. Probe never launches interactive browser logins in automated CI.

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
  max_output_tokens: 131072             # per-request admission ceiling; must be
                                        # >= llm.max_output_tokens when both are set
  max_request_bytes: 1048576
  max_request_chars: 500000
  max_input_tokens: 128000
```
- **`mode: soft`**: Halts subsequent requests once cumulative recorded spend reaches `max_usd`.
- **`mode: strict`**: Enforces strict upfront output token reservations against locked organization rate cards.

> **Dollar enforcement requires known cost or usable pricing.** Unknown cost
> cannot be treated as zero or used to certify a dollar limit. Strict mode needs
> a usable rate card for upfront reservations; use non-dollar call, tool and time
> ceilings as well when pricing is unavailable.

> **Two retry layers exist.** `budget.max_retries` counts sas2r-level retries.
> `llm.max_tries` controls transport attempts inside ellmer. sas2r defaults that
> setting to **1**; keep it at 1 for parallel translation so hidden transport
> retries do not bypass per-attempt coordination. If both `max_tries` and
> `max_parallel_translations` exceed 1, startup stops and asks you to set one to 1.

### Cost Provenance
`sas2r` records cost under five provenance states: `billed_amount`, `contract_estimate`, `catalog_estimate`, `incomplete_estimate`, or `unknown`. `sas2r` owns no built-in fallback price table; unknown pricing remains `unknown` and is never estimated from arbitrary hard-coded rates.

Per-agent tool limits are separate from run-level `usage_limits`. The shipped
translator, reviewer and fixer each allow 30 tool calls per invocation, shared
across their tools. A run reporting `max_tool_calls=unlimited` still has these
agent limits. See [agent tool limits](running-migrations.md#agent-tool-limits)
for role overrides and the behavior when an allowance is exhausted.

---

## 5. Model Capabilities & Fallbacks

### Declaring what your endpoint supports (`capabilities:`)

Capabilities resolve to `supported`, `unsupported`, or `unknown`. Startup can
verify explicitly requested model parameters. Structured output and tool calling
still require declarations; the runner does not assume them on `unknown`. No provider in the registry ships a `tool_calling`
default other than `unknown`, so unless you declare it, every agent unit is
skipped with a `tool_calling_unavailable` flag and the run still exits
successfully:

```yaml
llm:
  provider: deepseek
  model: deepseek-flash
  capabilities:
    tool_calling: native        # required to run the agent layer at all
```

Declare only what the selected model **and connector** support. DeepSeek uses
`fallback` on the current Chat Completions route; do not infer native-schema
support from another API or provider.

### Reasoning support in the recommended profiles

Checked against the installed ellmer 0.4.2 connector code on September 12, 2026:

| Provider/route | What happens to `reasoning_effort: high` |
|---|---|
| OpenAI, GPT-5.6 Terra | Forwarded to the Responses API reasoning effort. |
| Anthropic, Claude Sonnet/Opus 4.6 | Enables `thinking: {type: adaptive}` and `output_config.effort: high`. |
| Gemini / Vertex, Gemini 3 thinking models | Forwarded as `thinkingConfig.thinkingLevel: high`. |
| Posit, Claude route | Uses the Anthropic mapping above. Posit's OpenAI-compatible route drops effort. |
| DeepSeek | Dropped by this connector. Current DeepSeek models default to thinking enabled at high effort. |
| Azure / Bedrock / Databricks / Snowflake | Dropped by these connectors; only endpoint/model defaults apply. |
| Ollama | Model-dependent; the connection example is not a verified reasoning profile. |
| GitHub | Retired; do not start new translation runs with it. |

Sources for model behavior:
[OpenAI Terra](https://developers.openai.com/api/docs/models/gpt-5.6-terra),
[Gemini thinking](https://ai.google.dev/gemini-api/docs/generate-content/thinking),
[Claude thinking](https://platform.claude.com/docs/en/build-with-claude/thinking-steering-and-cost),
[DeepSeek thinking](https://api-docs.deepseek.com/guides/thinking_mode/).
Use reasoning-capable models in every active `tiers` entry: declarations are
shared across tiers, while startup verification checks each active model separately. These settings encourage reasoning; they do not prove
SAS-to-R equivalence. Defaults can change, and a withheld effort setting does not mean reasoning is off.

### Automatic startup settings verification

`sas_translate()` checks each active agent's explicitly configured parameters
before starting agent work. The same check also protects standalone agent calls.
Set the desired level and output allowance; sas2r applies verified capabilities
in memory without rewriting `_sas2r.yml`:

```yaml
llm:
  reasoning_effort: high
  max_output_tokens: 32768
```

This fragment belongs inside a complete provider profile above. Keep the
`structured_output` and `tool_calling` declarations: the settings probe is a
structured ping with no tools, so it does not discover tool support.

- **Unknown reasoning support:** send an invalid effort level, require a provider
  rejection identifying that setting, then test the exact requested level. An
  endpoint accepting the invalid level may be ignoring the field; stop with
  support still unknown. An independently verified `supported` override skips
  this negative control, but still requires the positive settings ping.
- **Known unsupported:** stop before transport when the user requests the setting.
  An ellmer warning that a required parameter is being ignored also stops the run.
- **Accepted settings:** continue with those values. Required settings are never
  removed during the optional-parameter retry. Rejection later in translation
  also stops the run.
- **Timeout, authentication failure, truncation, or exhausted probe budget:** do
  not infer unsupported capability or cache a success. Transient failures use
  the existing bounded probe retry policy; unresolved checks stop the run.

A fresh adapter probes once per distinct active model/settings profile. Successful
checks are cached only in that adapter session, keyed by endpoint, model, API and
ellmer versions, capability declarations, and exact parameter values. A new
`sas_llm()` adapter starts fresh. Startup logs `probing`, `verified`, `cached`, or
`provider_defaults`; requested/effective values are recorded in
`<out_dir>/.sas2r/llm_log.jsonl`. Probe attempts use the same usage ledger and
budget as translation. An already exhausted run budget permits no probe or agent
calls. `sas_preflight()` and constructing `sas_llm()` remain offline.

Explicit `temperature` and `top_p` settings are also tested and required. An agent
spec overrides project parameter defaults where it speaks; the shipped
`temperature: 0` remains optional unless temperature was explicitly configured.
An unknown capability may therefore still withhold that implicit default, which
is recorded as `withheld_parameters` in the audit log.

These checks establish connector forwarding and request acceptance, not the
amount or quality of a model's internal reasoning. The negative control applies
to reasoning; acceptance of other parameters does not prove their semantics on
an arbitrary compatible endpoint. DeepSeek on ellmer 0.4.2 must use its server
reasoning default because that connector cannot forward an explicit effort.

Upgrading to ellmer 0.5.0 does not remove that DeepSeek limitation: its
[versioned parameter mapping](https://github.com/tidyverse/ellmer/blob/v0.5.0/R/provider-deepseek.R#L69-L83)
includes `max_tokens` but omits `reasoning_effort`. The startup check therefore
remains necessary on both versions; upgrading the connector alone does not
repair sas2r's earlier omission of unconfirmed settings.

Token summaries distinguish known total input/output usage from cached and
reasoning categories. Reasoning is already included in total output where the
provider counts it there; unavailable usage remains unknown. See the
[usage evidence guide](migration-evidence.md#coverage-limits-and-reuse).

### Request timeout and retries (`timeout_seconds`, `max_tries`)

Each HTTP request is bounded by ellmer's `ellmer_timeout_s` option, defaulting
to 300 seconds, with `ellmer_max_tries` limiting HTTP attempts; sas2r defaults to 1. A
single long model response can exceed the timeout and fail mid-stream with
`sas2r_llm_timeout`. Increasing `max_tries` adds
transport attempts beneath sas2r retries and can multiply elapsed time and spend.
Values above 1 require `migration.max_parallel_translations: 1`; incompatible
effective settings raise a startup configuration error.

```yaml
llm:
  timeout_seconds: 900     # per HTTP request, not per run
  max_tries: 2             # ellmer-level attempts per request
```

`timeout_seconds` applies to one HTTP request; bound total runtime with
`budget: max_wall_time:`. `max_tries` is the ellmer-level companion to
`budget: max_retries:`. The settings limit different retry layers; increasing
both can multiply attempts and spending.

### Other behaviour

- If a provider rejects structured JSON output natively, `sas2r` seamlessly falls back to schema-guided repair prompts.
- All credential values, tokens, and authorization headers are redacted from console output, logs, and `_sas2r.lock`.

---

## 5. Data & Model Privacy Boundary

`sas2r` maintains a declared, bounded boundary between local data and remote language models:

- **Default Model Evidence**: Source and generated code, input schema metadata, helper interfaces and execution diagnostics. Reference comparison summaries, digests, values and reports do not enter code-writing requests or tools; project overrides cannot restore the comparison tool.
- **Bounded Candidate Evidence**: `agent_evidence = "bounded"` permits capped candidate-output summaries and previews in execution diagnostics. `code_only` omits these previews. Complete reference comparisons remain in local reports and the public comparison API. Source inputs retain their input role even when also used as references.
- **Data Residency**: All dataset reading, writing, and execution take place in local R processes on your infrastructure; confirm the endpoint you configure meets your enterprise data residency obligations.

Source code, comments and errors can themselves contain patient information.
`code_only` does not de-identify them. Read the [full privacy guidance](model-privacy.md).
