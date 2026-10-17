# PI_PLAN：实现 pi agent 的脚本模式（`bin/gf pi`）

> 本文档是提供给 pi agent 脚本模式的任务规格书：把本文档作为输入交给 pi，
> 由 pi 完成实际编码。文档中的路径、模块名、命令均为约定，实现时必须严格遵守。
>
> 文中引用的 pi 代码均在 `~/git/pi` 检出（2026-10），形如
> `packages/ai/src/api/google-shared.ts:443` 表示该仓库中的文件与行号；
> 引用 goldfish 代码（如 `goldfish/liii/http.scm:82`）则指本仓库。

## 1. 目标

在 Goldfish Scheme 的 CLI（`bin/gf`）中新增 `pi` 子命令，用纯 Scheme 实现
[pi coding agent](https://github.com/earendil-works/pi) 的**脚本模式**（非交互模式），
最终做到 `bin/gf pi` 可以替代 `pi` 的脚本模式：

```bash
# print 模式：运行 agent，把最终 assistant 文本写到 stdout，然后退出
bin/gf pi --print "Summarize this repository"
git diff | bin/gf pi --print "Review this change"

# JSON 模式：向 stdout 写 JSONL 事件流，然后退出
bin/gf pi --mode json "Inspect this repository" > events.jsonl
```

两个前提约束（本任务的范围根基）：

1. **首个接入的模型是 Gemini**（Google Generative Language API）。provider 层按
   可扩展接口设计，Anthropic 作为后续独立任务（§5.7）。
2. **复用 `~/.pi/agent/` 下 pi 的既有配置**（settings.json、auth.json、
   models-store.json），用户不需要为 `bin/gf pi` 重新配置模型与密钥（§4）。
   对该目录**只读**，绝不写入。

**范围外（Non-goals）**：交互式 TUI、RPC 模式、extensions/MCP、codemode、packages、
themes、prompt templates、skills、图片输入、OAuth 凭据与刷新（auth.json 里的
`type:"oauth"` 条目本期不消费）、thinking 内容的请求与解析、会话写入 `~/.pi/agent/`、
跨平台支持（见下）。

**平台范围**：只需在当前这台 Linux 开发机上实现并通过验证即可，**不需要考虑跨平台**
（Windows/macOS、不同 shell、路径分隔符差异等均不在范围内；bash 工具假定 `/bin/bash`）。

## 2. CLI 规格

参照 pi 的参数解析（`packages/coding-agent/src/main.ts:112-122`）与文档
`packages/coding-agent/docs/cli.md`：

```
gf pi [options] [--] [@files...] [messages...]
```

| 选项 | 行为 |
|---|---|
| `-p`, `--print` | 一次性运行，最终 assistant 文本写 stdout 后退出 |
| `--mode text` | 文本输出；不强制一次性运行（本任务无 TUI，等同于 print） |
| `--mode json` | JSONL 事件流写 stdout 后退出 |
| `--model <id>` | 模型 ID（如 `gemini-3.8-flash`），解析规则见 §2.1 |
| `--provider <name>` | 限定 provider（首期只有 `google` 有实现） |
| `--proxy <url>` | 覆盖代理地址（`socks5h://...` 或 `http://...`），见 §5.6 |
| `--tools <list>` | 逗号分隔的工具列表（见 §6.1） |
| `--max-turns <n>` | 轮数上限，0 表示不限（goldfish 新增，见 §7.3） |
| `--timeout <seconds>` | bash 工具与单次 HTTP 请求的默认超时（goldfish 新增，见 §9） |
| `--no-session` | 纯内存会话，不落盘（首期所有会话都如此，选项仅为兼容） |
| `--system-prompt <text\|path>` | 替换默认系统提示词 |
| `--append-system-prompt <text\|path>` | 追加系统提示词（可重复） |
| `--` | 停止选项解析，使 prompt 可以以 `-` 开头 |
| `-h`, `--help` | 帮助后退出 |

**模式选择**（对齐 `main.ts:112-122`）：`--mode json` → JSON 模式；`--print` 或
stdin/stdout 任一非 TTY → print 模式；本任务不存在 TUI，其余情况也走 print 模式。

### 2.1 模型解析

对齐 pi 的初始模型优先级（`packages/coding-agent/src/core/model-resolver.ts:543-610`
的 `findInitialModel`），goldfish 简化为：

```
--model  >  GF_PI_MODEL 环境变量  >  settings.defaultModel（需 defaultProvider 有凭据）
```

- 未指定 provider 时取 `settings.defaultProvider`（本机为 `google`）。
- 模型 ID 匹配 models-store.json 中该 provider 的条目（§5.5）取元数据；
  找不到时**不报错**，当作自定义模型 ID 并向 stderr 发 warning，元数据用兜底值——
  对齐 pi 的 `buildFallbackModel`（`model-resolver.ts:296-307`）。
- 首期只做精确 ID 匹配，不做 fuzzy/多 provider 歧义消解。

**输入组装**（对齐 `packages/coding-agent/src/cli/initial-message.ts:20-43` 与
`cli/file-processor.ts:25-88`）：

- `parts = [stdin内容?, @file内容?, 第一条message]`，**无分隔符直接 join**；
  其余 message 作为后续 prompt 依次发送。
- stdin：非 TTY 时读取全部输入，`trim` 后为空则忽略（`main.ts:77-99`）。
- `@path`：相对当前目录解析，支持 `~` 展开；文本文件包裹为
  `<file name="/abs/path">\n内容\n</file>\n`；文件不存在/不可读 → 报错并退出码 1
  （`file-processor.ts:39,82`）。
- 首期只支持文本文件；`@图片` 明确不支持（Non-goals）。

## 3. JSON 模式事件流

参照 `packages/coding-agent/src/modes/json-event.ts`、`print-mode.ts:108-127` 与
`docs/json.md`。stdout 只写 JSONL（每行一个 JSON 对象，LF 结尾），诊断一律写 stderr。

### 3.1 session header（第一条记录）

字段定义见 `packages/coding-agent/src/core/session-manager.ts:43-50,1057-1083`：

```json
{"type":"session","version":3,"id":"<uuid>","timestamp":"2026-10-17T08:00:00.000Z","cwd":"/abs/cwd"}
```

- `version` 固定 `3`（`CURRENT_SESSION_VERSION`，`session-manager.ts:41`）。
- `id`：pi 用 uuidv7（`session-manager.ts:264-266`）；goldfish 允许用
  "时间戳+随机数"自造同格式 ID。
- `timestamp`：ISO 8601 UTC；`cwd`：`getcwd` 解析结果。

### 3.2 一次基础运行的事件序列

（`packages/agent/src/agent-loop.ts:163-321` 的事件发射点 + `agent-session.ts:1082-1145`）

```json
{"type":"agent_start"}
{"type":"turn_start"}
{"type":"message_start","message":{"role":"user","content":"...","timestamp":1733234401000}}
{"type":"message_end","message":{"role":"user","content":"...","timestamp":1733234401000}}
{"type":"message_start","message":{"role":"assistant","content":[],"stopReason":"pending","usage":{...},"timestamp":...}}
{"type":"message_update","usage":{...},"assistantMessageEvent":{"type":"text_delta","contentIndex":0,"delta":"Hello"}}
{"type":"message_end","message":{"role":"assistant","content":[...],"stopReason":"stop","usage":{...}}}
{"type":"turn_end","message":{"role":"assistant"},"toolResults":[]}
{"type":"agent_end","messages":[...],"willRetry":false}
{"type":"agent_settled","aborted":false}
```

有工具调用时插入（`agent-loop.ts:946-949`）：
`tool_execution_start{toolCallId,toolName,args}` →
`tool_execution_end{toolCallId,toolName,result,isError,durationMs?}` →
`message_start`/`message_end {message: toolResult}`。
（`tool_execution_update` 只在流式阶段出现。）

### 3.3 wire 细节

- `message_update` 不带完整消息快照：只发 `{type:"message_update", usage, assistantMessageEvent}`
  （`json-event.ts:46-61`）；`toolcall_start` 事件额外补 `{id, toolName}`
  （`json-event.ts:23-30`）。非流式阶段（§11 阶段六之前）允许省略 `message_update`。
- `timestamp` 字段是 Unix 毫秒；session header 的 `timestamp` 是 ISO 字符串——两者不同，勿混用。
- 退出码：事件流中出现 error/aborted 的 assistant 消息**不**改变退出码；
  只有进程本身抛错才非 0（`docs/cli-integration.md`；与 print 模式不同）。

### 3.4 消息对象形状

类型定义在 `packages/ai/src/types.ts`（AssistantMessage:570-600，Usage:451-472，
ToolResultMessage、UserMessage 同文件）。注意 pi 内部把工具调用 block 叫
**`toolCall`**，发往 Gemini 时转成 `functionCall` part（§5.4）：

- assistant：`{role:"assistant", content:[text|toolCall], stopReason, usage, timestamp, errorMessage?, ...}`
  - `stopReason ∈ "pending"|"stop"|"length"|"toolUse"|"error"|"aborted"|"deferred"`
    （`types.ts:474`）
  - content block：`{type:"text",text}` / `{type:"toolCall",id,name,arguments}`
- toolResult：`{role:"toolResult", toolCallId, toolName, content:[{type:"text",text}], isError, timestamp}`
- user：`{role:"user", content: "<字符串或block数组>", timestamp}`

## 4. 复用 `~/.pi/agent/` 配置（只读）

pi 的配置目录由唯一入口 `getAgentDir()` 决定（`packages/coding-agent/src/config.ts:605-610`）：
默认 `~/.pi/agent`，可被环境变量 `PI_CODING_AGENT_DIR` 重定向（测试靠它指向 fixture
目录）。目录下的派生路径见 `config.ts:613-655`。

goldfish 读取以下三个文件，**一律只读、不写、不锁**（pi 写入时用 proper-lockfile 与
0600 权限，`core/auth-storage.ts:37,71`——goldfish 不参与写竞争）：

### 4.1 settings.json

schema 定义在 `packages/coding-agent/src/core/settings-schema.ts:206-454`。goldfish 消费：

| 字段 | 用途 | 出处 |
|---|---|---|
| `defaultProvider` / `defaultModel` | 模型解析兜底（§2.1） | settings-schema.ts:210-211 |
| `defaultThinkingLevel` | 本期忽略（thinking 是 Non-goal），解析但不用 | settings-schema.ts:212 |
| `httpProxy` | 代理兜底（§5.6） | settings-schema.ts:394-396 |
| `retry` | 重试参数：`enabled`/`maxRetries`/`baseDelayMs`/`maxAgentDelayMs` | settings-schema.ts:55-72 |
| `defaultTools` | 工具选择：裸名替换默认集、`+name`/`-name` 增删 | settings-schema.ts:333-338 |

合并语义（`settings-manager.ts:126-147`）：全局 `~/.pi/agent/settings.json` 与项目
`<cwd>/.pi/settings.json` 递归深合并、project 叶子覆盖 global；`defaultTools` 单独按
裸名替换/`+`/`-` 规则合并。**注意 pi 的项目级 settings 受 trust 门控**
（`settings-manager.ts:366-369`），且 `httpProxy` 只从全局 settings 生效
（`core/http-dispatcher.ts` 调用点 `main.ts:597,875`）。goldfish 首期**只读全局
settings.json**，项目级 `.pi/settings.json` 支持作为后续任务。

本机实际值参考（2026-10）：`defaultProvider: "google"`、`defaultModel:
"gemini-3.8-flash"`、`defaultThinkingLevel: "medium"`、`httpProxy: "http://127.0.0.1:7890"`。

### 4.2 auth.json

格式定义（`core/auth-storage.ts:17`，类型在 `packages/ai/src/auth/types.ts:17-37`）：
顶层 `Record<providerId, Credential>`，`Credential` 两种：

```jsonc
{ "google": {"type": "api_key", "key": "..."} }          // ApiKeyCredential，字段名是 key
{ "kimi-coding": {"type": "oauth", "access": "...", "refresh": "...", "expires": 0} }  // 本期不消费
```

goldfish 只处理 `type === "api_key"`。key 值支持 pi 的配置插值语法
（`resolveConfigValue`，`auth-storage.ts:264-267`）：`$VAR`/`${VAR}` 插值环境变量；
`` !command `` 形式（执行 shell 命令取值）**本期不支持**，遇到则报错退出。
凭据解析优先级对齐 `packages/ai/src/auth/resolve.ts:49-96`：存储条目 `key` 优先，
否则探测环境变量 `GEMINI_API_KEY`（`providers/google.ts:9`）。

### 4.3 models-store.json

这是 pi 的远程模型目录缓存，不是用户配置（注释见
`packages/ai/src/models-store.ts:46`）。结构 `Record<providerId, {models: [...],
lastModified, checkedAt, etag}>`。goldfish 把它当作模型元数据只读数据源（§5.5）。
本机 `google.models` 有 23 个模型，含 `gemini-3.8-flash`
（contextWindow 1048576 / maxTokens 65536 / baseUrl 官方 v1beta）。

## 5. Provider 层：Gemini（首个接入）

参照 pi 的实现：`packages/ai/src/api/google-generative-ai.ts`（471 行）、
`google-shared.ts`（517 行，Gemini/Vertex 共享转换）、`providers/google.ts`。
pi 基于官方 SDK `@google/genai`，goldfish 直接讲 REST wire 协议（下述形状已按
SDK 的映射规则还原为原始 JSON）。

### 5.1 端点与鉴权

- 默认 baseUrl：`https://generativelanguage.googleapis.com/v1beta`
  （`providers/google.ts:8`）。baseUrl 需自带版本路径（pi 把 apiVersion 置空、
  版本烘进 baseUrl，`google-generative-ai.ts:353-357`）。
- 非流式（首期）：`POST {base}/models/{model}:generateContent`
- 流式（§11 阶段六）：`POST {base}/models/{model}:streamGenerateContent?alt=sse`
  （SDK path 模板见 SDK `index.mjs:17701`）
- 鉴权：请求头 `x-goog-api-key: <key>`（不是 query param；SDK 注入点
  `index.mjs:10973`）。key 来源见 §4.2。

### 5.2 请求体

pi 的 `buildParams`（`google-generative-ai.ts:369-429`）经 SDK 映射后的 wire 形状：

```jsonc
{
  "contents": [ /* 见 5.4 */ ],
  "systemInstruction": "<系统提示词纯字符串>",     // :391-394，Gemini 不支持中途 system
  "tools": [{"functionDeclarations": [
      {"name": "bash", "description": "...",
       "parametersJsonSchema": { /* 完整 JSON Schema */ }}   // google-shared.ts:382-403
  ]}],
  "toolConfig": {"functionCallingConfig": {"mode": "AUTO"}}, // :398-400
  "generationConfig": {
    "maxOutputTokens": 65536    // pi 仅在有值时发送（:382-384）；goldfish 首期省略
  }
}
```

- `parametersJsonSchema` 接受**完整 JSON Schema**（含 anyOf/const 等），不做
  Anthropic 式三件套裁剪（`google-shared.ts:382-403`）；§6.2 的 schema 两边通用。
- thinkingConfig **首期整个省略**：不请求 `includeThoughts` 时响应不含 thought part，
  最简单且符合 Non-goals。（pi 的三态逻辑见 `google-shared.ts:72-113`、
  `google-generative-ai.ts:431-471`，留待 thinking 任务。）

### 5.3 响应映射

非流式响应是单个 JSON 对象：

```jsonc
{"candidates": [{"content": {"role": "model", "parts": [...]}, "finishReason": "STOP"}],
 "usageMetadata": {"promptTokenCount": 0, "candidatesTokenCount": 0,
                   "thoughtsTokenCount": 0, "cachedContentTokenCount": 0, "totalTokenCount": 0},
 "responseId": "...", "modelVersion": "..."}
```

finishReason → pi stopReason（`mapStopReason`，`google-shared.ts:443-471`）：

| Gemini `finishReason` | pi `stopReason` |
|---|---|
| `STOP` | `"stop"`；**若本次响应含 functionCall 则改判 `"toolUse"`**（`google-generative-ai.ts:227-229`） |
| `MAX_TOKENS` | `"length"` |
| `SAFETY`/`RECITATION`/`BLOCKLIST`/`PROHIBITED_CONTENT`/`SPII`/`LANGUAGE`/`MALFORMED_FUNCTION_CALL`/`OTHER` 等 | `"error"`（errorMessage 带 raw 值） |

usage 映射（`google-generative-ai.ts:232-251`）：

```
input   = promptTokenCount - cachedContentTokenCount
output  = candidatesTokenCount + thoughtsTokenCount
cacheRead = cachedContentTokenCount
cacheWrite = 0                       // Gemini 无此概念（:239）
totalTokens = totalTokenCount        // Google 直接给，不需自加
```

### 5.4 function calling 往返

- **contents 的 role**：`"user"` 与 `"model"`（assistant→`model`、工具结果→`user`，
  `google-shared.ts:283,328`）。
- assistant 文本 part：`{"text": "..."}`；thinking part（`thought:true`）首期不出现、
  出现则跳过。
- 模型输出的工具调用 part（`google-shared.ts:269-276`）：
  `{"functionCall": {"name": "...", "args": {...}, "id": "..."}}`。
  **id 常缺失**：缺失或重复时生成 `<name>_<毫秒时间戳>_<计数器>`（`google-generative-ai.ts:195-201`）。
- 工具结果回传 part（`google-shared.ts:286-341`）：

  ```jsonc
  {"functionResponse": {
     "name": "<toolName>",
     "response": {"output": "<文本>"} ,   // 工具失败时改 {"error": "<文本>"}
     "id": "<toolCallId>"                  // Gemini 主版本 ≥3 需要显式 id
  }}
  ```

- `requiresToolCallId`（`google-shared.ts:167-174`）：模型主版本 ≥3（含
  gemini-3.8-flash）需在 functionCall 与 functionResponse 两端都带 id；id 归一化
  `[^a-zA-Z0-9_-] → "_"` 且截断 64（`google-shared.ts:197-200`）。
- **连续的工具结果合并进同一个 user turn**（`google-shared.ts:322-332`），即多个
  functionResponse part 放在一条 `role:"user"` contents 里。

### 5.5 模型元数据解析

解析顺序（兜底值对齐 pi 的自定义模型默认，`core/provider-composer.ts:248-249`）：

```
GF_PI_BASE_URL / GF_PI_CONTEXT_WINDOW / GF_PI_MAX_TOKENS（测试与覆盖用）
  > models-store.json 该 provider 条目下按 id 精确匹配
  > 兜底：baseUrl=官方 v1beta、contextWindow=128000、maxTokens=16384
```

### 5.6 网络与代理（SOCKS 是硬需求）

**访问 Gemini 必须经代理**。pi 侧的代理走 undici 的 `EnvHttpProxyAgent`
（`core/http-dispatcher.ts:81-113`），**只支持 http/https 代理、不支持 socks**；
其 settings 的 `httpProxy` 也只是写 `HTTP_PROXY`/`HTTPS_PROXY` 环境变量
（`http-dispatcher.ts:45-50`）。goldfish 走 cpr/libcurl（`3rdparty/curl-8.21.0`），
`CURLOPT_PROXY` 原生支持 `socks5://`/`socks5h://`，这是相对 pi 的能力增强。

- `(liii http)` 的 `http-get`/`http-post` 已有 `proxy` 参数（字符串关联表，
  `src/liii_http.cpp:118-125` 透传 cpr::Proxies；Scheme 侧
  `goldfish/liii/http.scm:82-96`）。用法：
  `(http-post url '() body headers '(("https" . "socks5h://127.0.0.1:1080")) ...)`
- 代理解析优先级：`--proxy` > `HTTPS_PROXY`/`ALL_PROXY` 环境变量 >
  `settings.json` 的 `httpProxy` > 直连。
- 推荐用户配置 `socks5h://`（域名解析也走代理）；http/https 代理同样支持。
- key/代理/请求体都**不允许**写入任何日志（stderr 只输出错误摘要）。

### 5.7 Anthropic（后续独立任务）

Anthropic Messages API 适配作为独立任务后置，参照
`packages/ai/src/api/anthropic-messages.ts`：请求头 `x-api-key` +
`anthropic-version: 2023-06-01`、工具用 `input_schema` 三件套、工具结果用
`tool_result` block、stopReason 映射表在其 1664-1690 行。provider 接口按此预留。

## 6. 工具层

工具声明结构参照 `packages/coding-agent/src/core/tools/tool-definition-wrapper.ts:8`
（name/description/parameters(JSON schema)/execute）。

### 6.1 工具集合与默认激活

- 工具全集：`read | bash | edit | write | grep | find | ls`（powershell 不做）。
  名称集合见 `packages/coding-agent/src/core/tools/index.ts:95`。
- **默认只激活 `["read","bash","edit","write"]`**（`agent-session.ts:3675`）；
  grep/find/ls 需 `--tools` 或 settings `defaultTools` 显式开启。
- goldfish 的 grep/find 用自实现（GNU grep/find/glob 或 Scheme 实现），不依赖
  ripgrep/fd 外部下载（pi 会从 GitHub Releases 拉 rg/fd 二进制，
  `packages/coding-agent/src/utils/tools-manager.ts:51-67`——该行为不复制）。
- 未知名调用 → error 结果 `Tool <name> not found`（`agent-loop.ts:720`）。

### 6.2 工具 schema 与行为规格

**截断常量全局统一**（`packages/coding-agent/src/core/tools/truncate.ts:11-13`：
2000 行 / 50KB；行数与字节是两个独立上限，先到先赢）：

| 工具 | 参数（必填加粗） | 关键行为（出处 `core/tools/<name>.ts`） |
|---|---|---|
| `read` | **path**；offset(1-based)、limit | 文本读，截断保头部 2000 行/50KB；续读提示 `[Showing lines A-B of TOTAL. Use offset=C to continue.]`；offset 越界报错（read.ts:160-199） |
| `bash` | **command**；timeout(秒) | 输出截断**保尾部** 2000 行/50KB；非零退出码不报错，返回 `isError:true` 结果并附 `Command exited with code N`（bash.ts:400-407）；超时报 `Command timed out after N seconds`（bash.ts:371-381）。**goldfish 偏差：timeout 必传**（pi 无默认超时），默认取 `--timeout` |
| `edit` | **path**、**edits[]**（每项 `oldText`/`newText`） | oldText 必须唯一且互不重叠；找不到/不唯一/重叠/无变化分别报错（edit-diff.ts:253-349）；所有 edits 对同一份原文匹配后倒序应用；成功返回 `Successfully replaced N block(s) in path.` |
| `write` | **path**、**content** | 自动创建父目录；返回 `Successfully wrote to path`（write.ts:65-88） |
| `grep` | **pattern**；path、glob、ignoreCase、literal、context、limit(默认100) | 输出 `relpath:123: text`，context 行 `relpath-120- text`；单行截 500 字符；上限 100 条匹配或 50KB（grep.ts:137-303） |
| `find` | **pattern**；path、limit(默认1000) | glob 匹配，输出相对路径，尊重 .gitignore；上限 1000 条或 50KB（find.ts:76-257） |
| `ls` | path、limit(默认500) | 字母序、目录加 `/` 后缀、含 dotfiles；上限 500 条或 50KB（ls.ts:60-109） |

- 路径解析：全部相对会话 cwd，支持 `~`（`core/tools/path-utils.ts:48`、
  `utils/paths.ts:76-107`）。
- 同一份 JSON Schema（`{type:"object",properties,required}`）直接用于 Gemini 的
  `parametersJsonSchema`；Anthropic 任务时再映射为 `input_schema`。

### 6.3 错误返回约定（两层机制，`packages/agent/src/agent-loop.ts:809-914`）

1. 工具**throw**（read/edit/write/grep/find/ls 的所有失败）→ 循环层捕获，转成
   `isError:true` 的 tool_result（`content:[{type:"text",text:错误消息}]`），**不让整轮失败**。
2. 工具**返回** `isError:true`（bash 非零退出码的做法）→ 同样标记错误，但已捕获的
   stdout/stderr 一并给模型。

## 7. Agent 循环

参照 `packages/agent/src/agent-loop.ts:163-321`（`runLoop`）：

1. 组装 system prompt + 消息历史，调 provider 得到 assistant 回复（推入历史）。
2. `stopReason === "toolUse"`：取出全部 toolCall，逐个执行，结果按序 push 进历史，
   回到 1。连续 toolResult 在发往 provider 前合并（§5.4）。
3. `stopReason === "stop"`：结束。
4. `stopReason === "length"`：**不执行任何 tool call**，全部标为 error 结果
   （`agent-loop.ts:267-270,478-503`），循环结束。
5. `stopReason === "error" | "aborted"`：循环结束。

### 7.1 系统提示词

参照 `packages/coding-agent/src/core/system-prompt.ts`（`buildSystemPromptSections`）：
分节结构 `preamble`（未包裹）+ 各节用同名 XML 标签包裹（`<tools>`、`<rules>`、`<cwd>` 等）：

- `preamble`（原文，system-prompt.ts:157-158）：
  "You are an expert coding assistant operating inside pi, a coding agent harness.
  You help users by reading files, executing commands, editing code, and writing new files."
  （goldfish 版可把 pi 替换为 goldfish，但保持结构一致）
- `<tools>`：每个激活工具一行 `- <name>: <promptSnippet>`（snippet 见各工具文件）。
- `<rules>`：各工具的 guideline + 固定两条（system-prompt.ts:141-143）：
  "Be concise in your responses"、"Show file paths clearly when working with files"；
  只有 bash 没有 grep/find/ls 时追加 "Use bash for file operations like ls, rg, find"（:114）。
- `<cwd>`：当前工作目录。
- 项目上下文文件（`core/resource-loader.ts:185`）：候选 `AGENTS.override.md`、
  `AGENTS.md`、`CLAUDE.md`，包裹为
  `<project_instructions path="...">\n内容\n</project_instructions>`。
- `--system-prompt` 替换 preamble；`--append-system-prompt` 追加 addendum 节。

### 7.2 错误重试

对齐 `packages/ai/src/utils/retry.ts` 与 `settings-defaults.ts:18-26`，参数取自
settings.json 的 `retry` 字段（§4.1，本机 `maxRetries: 3, baseDelayMs: 2000`）：

- 指数退避 `baseDelayMs × 2^(attempt-1)`，单次延迟封顶 `maxAgentDelayMs`（60s）。
- 可重试：HTTP `408/409/429/5xx`、网络错误、超时（`provider-retry.ts:22-35`；
  Google 错误体形状 `{"error":{"code":429,"message":"...","status":"RESOURCE_EXHAUSTED"}}`）。
- **不可重试**：quota/billing 类错误（`retry.ts:7-28` 的模式表）。
- abort 类错误不重试。

### 7.3 轮数上限（goldfish 新增，有意偏差）

pi **没有**最大轮数上限（全仓库仅测试代码有 MAX_TURNS）。goldfish 默认
`--max-turns 200` 防失控循环，`0` 表示不限（与 pi 一致）；`--timeout` 是进程级兜底。

## 8. print 模式输出与退出码

参照 `packages/coding-agent/src/modes/print-mode.ts:139-165`：

- 取最后一条消息，`role === "assistant"` 时：遍历 content，**只对 `type === "text"`
  的 block 逐个输出，每个后加 `\n`**（toolCall block 不输出）。
- `stopReason === "error" | "aborted"`：stderr 打 `errorMessage`，**stdout 无输出**，
  退出码 1。
- `stopReason === "length"`：部分文本照常输出到 stdout，stderr 打
  "Response was truncated: the model reached its output token limit."，退出码 1。
- 进程异常 → 退出码 1。

## 9. 超时规范（强制要求，防止卡死）

**执行每一个命令、每一次 IO 都必须设置超时。** pi 的对应实现：provider 请求层
SDK 默认超时 600000ms=10 分钟（`packages/ai/src/types.ts:193-196`）；bash 工具
timeout 可选且无默认值（`core/tools/bash.ts:40-43`）——**goldfish 不复制这一点**。

| 场景 | goldfish 规定 | 超时后行为 |
|---|---|---|
| `bash` 工具 | `(liii subprocess)` `run` **必传** `:timeout`；默认 `--timeout`（120s），工具参数可覆盖但不得超过进程级上限 | 该次 tool_result 为 `Command timed out after N seconds`（isError），agent 可继续 |
| 单次 HTTP 请求（非流式） | `(liii http)` timeout 参数，默认 300s，`--timeout` 可调 | 按 provider 错误处理，进入 §7.2 重试 |
| 流式 HTTP 的**读空闲**超时 | 实现于读取循环 | 同上 |
| 整次运行 | `--timeout` 进程级兜底（默认 0=不限，靠外层 `timeout` 命令包裹） | 强制退出，退出码 1 |
| 用 pi 脚本模式开发本任务 | pi 执行的每条命令都用 shell `timeout` 包裹（见 §10 示例） | 超时按失败处理，重试不超过 2 次 |

- 超时错误必须结构化（进入事件流或 stderr），不允许静默挂起。
- 任何"无超时参数"的调用路径不允许合入（评审按此检查）。

## 10. 测试方法

```bash
# 1. 构建
timeout 600 xmake b goldfish

# 2. 格式化
bin/gf fmt

# 3. 核心库测试
timeout 120 bin/gf tests/goldfish/liii/pi-test.scm

# 4. CLI/工具层测试
timeout 300 bin/gf test tools/pi/tests/liii/goldpi-args-test.scm

# 5. 更新函数文档索引（新增/修改测试文件后）
bin/gf doc --build-json
```

- **端到端测试一律指向本地 stub server**（测试自身拉起，`GF_PI_BASE_URL` 指向它；
  配置目录用 `PI_CODING_AGENT_DIR` 指向测试 fixture），保证无网络、无真实密钥、
  不卡 CI；真实 API 冒烟测试只手动执行且必须带 `timeout` 与 `--proxy`。
- fixture 配置目录包含：最小 `settings.json`、`auth.json`（假 key）、
  `models-store.json`（1-2 个模型条目）。
- stub server 至少覆盖：正常回复、functionCall 回复、`MAX_TOKENS`、`SAFETY`、
  HTTP 429（验证重试）、SSE 事件流（流式阶段）。
- Adhoc 测试一律写进 `tests/` 目录下的既有测试文件（禁止 /tmp 临时文件、禁止 `bin/gf -e`）。

## 11. 分阶段任务拆分（TDD）

每个阶段都按固定节奏：**先写测试（进 `tests/`，确认后再实现）→ 实现 →
`bin/gf fmt` → 跑相关测试**。提交规范见 §11.1。

### 11.1 提交规范（强制）

**提交越小越好**：一个提交只做一件事——一个工具、一组函数、或一个阶段中的一个
验收点。一个大阶段应拆成多个小提交，每个提交独立可审、可回退。

**每个提交提交前必须依次通过以下检查，任何一步失败不得 commit**：

```bash
# 1. 构建（后续测试依赖最新二进制）
timeout 600 xmake b goldfish

# 2. 格式化：先 fmt，再确认无剩余 diff
timeout 120 bin/gf fmt
git diff --exit-code

# 3. 静态度量检查（行数限制与树深度限制）
timeout 120 bin/gf check

# 4. 本提交触及范围的测试
timeout 120 bin/gf tests/goldfish/liii/pi-test.scm
timeout 300 bin/gf test tools/pi/tests/liii/<本提交相关的测试>.scm
```

- `bin/gf check` 的度量上限是硬约束：函数过长或嵌套过深就继续拆小，这同时也是
  "提交越小越好"在代码层面的对应物。
- 测试与实现放在同一个提交里（测试先行可在工作区完成，但入库时同commit）。
- 提交信息格式：`[x_y] 简短描述`，单行。

| 阶段 | 状态 | 内容 | 验收标准 |
|---|---|---|---|
| 一 | 已完成 | CLI 骨架：注册 `pi` 子命令（`gfproject.json` + `tools/pi/`）、§2 参数解析、模式选择、stdin/@file 组装 | `bin/gf pi --help` 正确；prompt 组装单元测试覆盖 join 规则与 `--` |
| 二 | 已完成 | 工具层：read/bash/edit/write 默认集 + grep/find/ls（§6.2 全部行为与错误文案、截断常量） | 无网络情况下全部可用；每个工具含超时路径测试 |
| 三 | 已完成 | 配置层（§4）：settings/auth/models-store 只读解析，`PI_CODING_AGENT_DIR` 重定向 | fixture 目录下 provider/model/key 解析正确；`$VAR` 插值与 `!command` 报错路径有测试 |
| 四 | 已完成 | Gemini provider（§5，非流式）：请求构造、代理（§5.6）、finishReason/usage 映射、重试 | stub server 下请求体断言通过；socks5 代理参数透传断言；429 触发 3 次退避重试 |
| 五 | 已完成 | Agent 循环 + 系统提示词 + print 模式（§7、§8） | 多轮 functionCall 端到端；退出码规则逐条验证；AGENTS.md/CLAUDE.md 注入 |
| 六 | 已完成 | JSON 模式事件流（§3，非流式子集：无 message_update） | 事件序列与 §3.2 一致，jq 逐行可解析 |
| 七 | 待执行 | 流式（SSE）：`:streamGenerateContent?alt=sse` + `(liii sse)` 解码 + `message_update`/`tool_execution_update` | 能输出 text_delta；空闲超时生效 |
| 八 | 待执行 | Anthropic provider（§5.7，独立任务）；会话持久化与 `--continue`（独立任务） | 各自的验收标准届时补充 |

## 12. 文件布局（约定，必须遵守）

```
gfproject.json                        # 注册 "pi" 子命令
tools/pi/liii/goldpi.scm              # CLI 入口，模块 (liii goldpi)
tools/pi/liii/goldpi-args.scm         # 参数解析
tools/pi/liii/goldpi-config.scm       # ~/.pi/agent 配置读取（settings/auth/models-store）
tools/pi/liii/goldpi-provider.scm     # Gemini provider 适配层
tools/pi/liii/goldpi-agent.scm        # agent 循环与事件发射
tools/pi/liii/goldpi-tools.scm        # 内置工具实现
tools/pi/tests/                       # CLI/工具层测试（bin/gf test 运行）
goldfish/liii/pi.scm                  # 可复用核心库（provider/agent/工具），模块 (liii pi)
tests/goldfish/liii/pi-test.scm       # 核心库测试
```

分层原则：`goldfish/liii/pi.scm` 只含可复用逻辑（无 CLI 副作用），
`tools/pi/liii/goldpi*.scm` 只做参数解析、组装与输出。

**实现边界**：一般来说，只需要在 `tools/` 下面写 Scheme 即可——优先复用现有库：
`(liii http)`（流式 `stream`+`callback` 与 `proxy` 关联表，`goldfish/liii/http.scm:82-122`）、
`(liii sse)`（SSE 解码）、`(liii subprocess)`（`:timeout`/`:cwd`）、`(liii json)`、
`(liii path)`（`path-glob`/`path-rglob`，`find` 工具可直接复用）。

**如果有必要，可以改 goldfish 本身**（`src/` 下的 C/C++ 胶水，或既有 `(liii *)` 库），
但仅限现有库确实缺少必需原语、且 Scheme 层无法绕过时——例如流式读取的空闲超时
若在 Scheme 层做不出来，才考虑在 `src/liii_http.cpp` 加 C 原语。此类修改必须：

1. 在提交信息中说明理由（为什么现有库不够）；
2. 在 §13 偏差清单中登记；
3. 保持对既有调用方零破坏（不改既有函数签名，只新增）。

## 13. goldfish 与 pi 的有意偏差清单

| # | 偏差 | 理由 |
|---|---|---|
| 1 | bash 工具 timeout 必传，默认 120s | pi 无默认超时；本项目强制防卡死（§9） |
| 2 | 默认 `--max-turns 200` | pi 无上限；防失控循环（§7.3） |
| 3 | 首期非流式 `:generateContent`，事件流无 delta | pi 恒流式 `:streamGenerateContent`；goldfish 先跑通再上流式（§11 阶段四/七） |
| 4 | grep/find 自实现，不下载 rg/fd | 免外部二进制依赖（§6.1） |
| 5 | 只读全局 settings.json，不读项目级 `.pi/settings.json` | 项目级受 pi trust 门控，首期不引入 trust 概念（§4.1） |
| 6 | auth.json 只消费 `type:"api_key"`，`$VAR` 支持而 `` !command `` 不支持 | 不执行任意命令、不刷新 OAuth（§4.2） |
| 7 | `generationConfig.maxOutputTokens` 首期省略 | 用模型默认上限（§5.2） |
| 8 | 不注入 `PI_*` 会话环境变量到 bash 工具 | 减少实现面（`core/tools/bash.ts:194-208` 对应功能后置） |
| 9 | 会话 id 允许非 uuidv7 的同格式自造 ID | s7 生态无现成 uuidv7（§3.1） |
| 10 | 代理支持 `socks5h://`（pi/undici 不支持 socks） | libcurl 原生能力，且是实际网络环境的硬需求（§5.6） |
| 11 | 只在当前 Linux 开发机上实现与验证，不考虑跨平台 | 任务只需要本机可用；Windows/macOS、非 bash shell 不在范围内（§1 平台范围） |
