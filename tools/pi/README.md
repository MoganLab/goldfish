# README：`bin/gf pi` 命令行使用说明

> **最终目标：`bin/gf pi` 可以替代 pi coding agent 的脚本模式**（print 模式与
> JSON 模式）——用户在脚本与管道中原来写 `pi -p "..."`、`pi --mode json "..."`
> 的地方，直接换成 `bin/gf pi ...` 即可，行为兼容、配置复用（`~/.pi/agent/`）。
>
> 本文档由真实冒烟测试验证整理：标注【可用】的示例均在开发机上实际运行通过；
> 尚不能运行的用法收入文末 TODO，随迭代更新。
> 更新方式：探索新的用法并实际验证后，把示例从 TODO 移入正文并注明验证日期。

## 前置条件

```bash
# 构建
timeout 600 xmake b goldfish

# 配置复用 ~/.pi/agent/（只读）：settings.json（defaultProvider/defaultModel/httpProxy/retry）、
# auth.json（api_key / ANTHROPIC_AUTH_TOKEN 等环境变量兜底）、models-store.json（模型元数据）
# 访问 Gemini 需要代理：--proxy > HTTPS_PROXY/ALL_PROXY 环境变量 > settings.json httpProxy > 直连
# 重试：可重试 provider 错误（429/5xx/408/409/网络错误）使用 retry.provider.maxRetries 预算
# （本机=12），未配置该嵌套节时回退 retry.maxRetries（缺省 3）
```

## 基本用法【可用】

```bash
# 一次性运行：最终 assistant 文本写 stdout 后退出
timeout 300 bin/gf pi -p "用一句话说明这个仓库是做什么的"

# 管道输入（stdin 非终端时自动读入，与 prompt 无分隔符拼接）
git diff | timeout 300 bin/gf pi -p "Review this change"
echo "Goldfish Scheme 是一个 Scheme 解释器" | timeout 300 bin/gf pi -p "把这句话翻译成英文，只输出译文"

# @文件：文件内容包裹为 <file name="..."> 块注入 prompt（支持 ~ 展开）
timeout 300 bin/gf pi -p @CLAUDE.md "这个项目的提交信息格式是什么？"
```

## 输出模式【可用】

```bash
# JSON 模式：JSONL 事件流写 stdout（默认走流式路径，含 message_update 增量事件）
timeout 300 bin/gf pi --mode json "列出当前目录结构" > events.jsonl

# 事件类型序列
jq -r .type events.jsonl
# session / agent_start / turn_start / message_start / message_update... / message_end / turn_end / agent_end / agent_settled

# 工具执行事件
jq -r 'select(.type=="tool_execution_start") | "\(.toolName) \(.args)"' events.jsonl
```

## 工具【可用】

```bash
# 默认激活 read/bash/edit/write/web_search；grep/find/ls 需显式开启
# （web_search 由 web-search 扩展注册时声明 :default-active? #t 并入默认集，核心不写死）
timeout 300 bin/gf pi -p --tools read,bash,grep,find,ls "统计 tests/ 下测试文件数量"

# web_search 已默认启用，无需 --tools 指定（详见内置扩展章节）
timeout 280 bin/gf pi -p "搜索 Goldfish Scheme 最新版本并给出引用来源"

# 临时关闭 web_search（黑名单或增量均可）
timeout 280 bin/gf pi -p --exclude-tools web_search "只回答：1+1等于几"
timeout 280 bin/gf pi -p --tools "-web_search" "只回答：1+1等于几"

# 黑名单排除工具：从默认工具集或指定工具集中排除（快速变为只读审计模式）
timeout 300 bin/gf pi -p --exclude-tools bash,write,edit "你有哪些可用工具？列出工具名称"

# 白名单与黑名单共存时排除优先（--tools 与 --exclude-tools 组合）
timeout 300 bin/gf pi -p --tools read,bash,edit --exclude-tools bash "你有哪些可用工具？列出工具名称"

# 彻底关闭工具调用，变为纯文本对话模式（--no-tools 与 --no-builtin-tools 等价）
timeout 300 bin/gf pi -p --no-tools "1+1等于几？直接回答数字"
timeout 300 bin/gf pi -p --no-builtin-tools "1+1等于几？直接回答数字"

# 与 --tools / --exclude-tools 组合时以关闭优先
timeout 300 bin/gf pi -p --tools read,bash --no-tools "1+1等于几？直接回答数字"

# bash 工具多轮调用
timeout 300 bin/gf pi -p --tools bash "运行 echo hello 并把输出原样告诉我"

# write + read 往返（write 自动创建父目录）
timeout 300 bin/gf pi -p --tools read,bash,edit,write "在 /tmp/demo/ 下写 hello.txt 内容为 hi，然后读回来告诉我"

# 长输出截断：bash 输出保尾部 2000 行 / 50KB
timeout 300 bin/gf pi -p --tools bash "运行 seq 1 5000 并告诉我最后 4 行"
```

## 会话【可用】

```bash
# 默认持久化会话（session JSONL）
timeout 300 bin/gf pi -p "记住暗号：金鱼42号"

# 继续上一会话（--continue / -c），可多级连续
timeout 300 bin/gf pi -c -p "刚才的暗号是什么？"
timeout 300 bin/gf pi -c -p "继续上一轮的话题"

# 选择并恢复历史会话（--resume / -r）：默认当前目录，Tab 切换全部项目；
# 输入模糊搜索，Enter 恢复，Esc 取消；Ctrl+N 仅看命名会话，
# Ctrl+P 显示文件路径，Ctrl+S 切换最近时间/相关度排序。
bin/gf pi --resume

# 指定会话存储目录（优先级：--session-dir > PI_CODING_AGENT_SESSION_DIR
# > settings.json 的 sessionDir > 默认 <agent-dir>/sessions/<encoded-cwd>/）
timeout 300 bin/gf pi --session-dir ~/my-sessions -p "记住暗号：金鱼42号"
timeout 300 bin/gf pi --session-dir ~/my-sessions -c -p "刚才的暗号是什么？"

# 纯内存运行，不写盘
timeout 300 bin/gf pi --no-session -p "一次性问题"
```

## TUI bash 模式与上下文注入【可用】（任务 1134，对齐 pi）

交互式 TUI（`bin/gf pi` 无参进入）中，整行以 `!` 开头的输入不发给模型，
而是直接经 bash 执行，输出显示在绿色边框卡片内（Ctrl+O 展开/折叠）：

```text
!ls -la                 # 执行并显示；命令与输出作为 user 消息注入后续 LLM 上下文
!!git status --short    # 仅执行显示（暗色卡片），不注入上下文（excludeFromContext）
!                       # 命令为空时回退为普通消息提交
```

注入格式对齐 pi 的 `bashExecutionToText`：`Ran \`cmd\`` + 围栏输出 +
非零退出码行；长输出复用 bash 工具的保尾 2000 行 / 50KB 截断策略。
会话文件以 `bashExecution` 角色落盘（`--continue` / 压缩摘要同样生效）。
agent 正在流式输出时结果延迟落盘，下一条消息才可见（对齐 pi 的 pending 语义）。
同一 TUI 运行内复用同一会话文件，跨消息保持上下文连续。


## 扩展机制与插件生态（Extension）【对齐原生 pi】

`bin/gf pi` 引入了与原生 pi 概念模型全面对齐的 **轻量纯 Scheme Extension 机制**。
无需引入 Node.js 运行时或外部进程，扩展全部由 Goldfish Scheme 编写，自包含且轻量。

### 1. 发现机制：目录约定即生效

用户与开发者**只要把相关的 `.scm` 插件文件（或带有 `index.scm` 的目录）放在对应目录下**，启动时自动扫描发现并加载，无需手动注册：

| 扩展类型 | 存放位置 | 说明 |
|---|---|---|
| **仓库内置扩展** | `tools/pi/extensions/` | 随 Goldfish 安装包直接分发内置（安装到 `share/goldfish/tools/`），开箱即用 |
| **用户全局扩展** | `~/.pi/agent/extensions/*.scm` 或 `*/index.scm` | 用户所有项目通用的全局插件 |
| **项目局部扩展** | `.pi/extensions/*.scm` 或 `*/index.scm` | 仅在当前项目生效的定制插件 |
| **显式指定加载** | `bin/gf pi -e /path/to/my-ext.scm` | 命令行 `-e` / `--extension` 快速调试单个插件脚本 |

### 2. 常用内置扩展（随包分发，开箱即用）

为保证日常编码体验的高效与开箱即用，最常用的 3 个插件随 Goldfish 安装包内置：
- **`web-search`**：注册 `web_search` 工具（联网搜索，默认启用，实现移植自 ttttmr/pi-web-search）；
- **`simplify`**：审查未提交代码的清晰度与可维护性，提供代码简化建议与 `--apply` 应用；
- **`code-review`**：审查工作区或暂存区的代码变更并输出结构化审查清单。

```bash
# simplify：代码简化审查
timeout 300 bin/gf pi -p --simplify
timeout 300 bin/gf pi -p --simplify --staged

# code-review：代码审查
timeout 300 bin/gf pi -p --code-review
timeout 300 bin/gf pi -p --code-review --staged
```

#### web-search：联网搜索工具【可用】（任务 040-043，扩展化 2026-10-20）

移植自 [ttttmr/pi-web-search](https://github.com/ttttmr/pi-web-search)，以内置扩展形式提供
`web_search` 工具（`extensions/web-search.scm` 注册，实现库 `(liii pi web-search)`）。
**默认启用**（扩展注册时声明 `:default-active? #t` 并入默认集，核心默认集不写死扩展
工具名），无需 `--tools` 指定；`--exclude-tools web_search` 可关闭；
settings.json 显式 `defaultTools` 完整列表时以用户配置为准（不合并扩展默认）。

```bash
# 默认已启用，直接联网搜索（Gemini Google Search grounding，回答附 Sources 引用）
timeout 280 bin/gf pi -p "搜索 Goldfish Scheme 最新版本并给出引用来源"

# JSON 模式事件：tool_execution_start（query/urls）→ tool_execution_end（sources/details）
timeout 280 bin/gf pi --mode json -p "搜索 R7RS-small 与 R7RS-large 的区别" | grep tool_execution
```

后端与凭据（优先级：显式参数 > `web-search.json` > 环境变量 > config）：

| 后端 | 凭据 | base-url | 说明 |
|---|---|---|---|
| `google`（默认） | `GOOGLE_API_KEY`/`GEMINI_API_KEY` 或 auth.json 的 google 条目 | `GOOGLE_BASE_URL`/`GEMINI_BASE_URL`/`GF_PI_BASE_URL` | Gemini Search Grounding（默认模型 `gemini-2.5-flash`） |
| `deepseek` | `DEEPSEEK_API_KEY` 或 auth.json 的 deepseek 条目 | `DEEPSEEK_BASE_URL` | 独立 Search + Fetch API，支持 urls 附带页面拉取 |

```bash
# 全局后端切换：~/.pi/agent/web-search.json（可用 PI_WEB_SEARCH_CONFIG 指定路径）
# {"provider": "deepseek", "model": "gemini-2.5-flash"}
echo '{"provider": "deepseek"}' > ~/.pi/agent/web-search.json

# 或环境变量切换（不落盘）
GF_PI_WEB_SEARCH_PROVIDER=deepseek timeout 280 bin/gf pi -p "搜索 DeepSeek V4 的发布日期"
```

工具参数：`query`（必填，搜索词）与 `urls`（可选，随搜索一并分析的 URL，最多 20 个）。
代理复用 `--proxy > HTTPS_PROXY/ALL_PROXY`；访问 Gemini 官方端点通常需要代理。

#### simplify：代码简化审查【可用】

审查未提交（或指定 ref）的代码变更，输出可维护性/清晰度改进建议（只读，不改代码）。

```bash
# 审查工作区未提交变更
timeout 300 bin/gf pi -p --simplify

# 审查已暂存变更（git diff --cached）
timeout 300 bin/gf pi -p --simplify --staged

# 审查相对某个 ref 的变更
timeout 300 bin/gf pi -p --simplify --ref origin/main

# 建议确认后，在变更行范围内自动应用简化（写回文件）
timeout 300 bin/gf pi -p --simplify --apply
```

约束：`--apply` 需与 `--simplify` 同用；`--staged`/`--ref` 需与 `--simplify` 或
`--code-review` 之一同用；与 `--code-review` 互斥。

#### code-review：代码变更审查【可用】

对工作区（或暂存区、指定 ref）的变更逐文件输出结构化审查清单（问题等级与位置）。

```bash
# 审查工作区未提交变更
timeout 300 bin/gf pi -p --code-review

# 审查已暂存变更
timeout 300 bin/gf pi -p --code-review --staged

# 审查相对某个 ref 的变更
timeout 300 bin/gf pi -p --code-review --ref HEAD~3

# 交互注入：write/edit 后自动追加当前语言的审查清单到系统提示词
# （--auto-review，与 --code-review 互斥）
timeout 600 bin/gf pi -p --auto-review "修复 src/app.scm 里的空指针问题"
```

约束：`--staged`/`--ref` 需与 `--code-review` 或 `--simplify` 之一同用；
`--auto-review` 与 `--code-review` 互斥。

#### multikey：密钥池与 429/403 自动轮换【可用】（任务 033，扩展化 2026-10）

移植自 [kslamph/multikey](https://github.com/kslamph/multikey)，以内置扩展形式提供
（`extensions/multikey.scm` 注册，实现库族 `(liii pi multikey pool/load/policy/config)`）。检测
`~/.pi/agent/multikey.json`：命中当前 provider 的 pool 时自动注入
`request-key` / `rotate-key` 钩子，正常请求固定使用当前 key，429/403 时
冷却当前 key 并切换下一个（切换日志与重试日志共用通道：CLI 写 stderr，
TUI 转为界面卡片）；核心请求路径只依赖通用钩子接口 `(liii pi ai keyring)`，
不依赖任何具体密钥池实现。

```bash
# 自动检测 ~/.pi/agent/multikey.json：配置了 kimi-coding 多 key pool 时，
# 正常请求固定使用当前 key；429/403 时冷却当前 key 并切换下一个（首个 key 不打日志）
timeout 180 bin/gf pi --provider kimi-coding -p "统计 tests/ 目录下的测试用例总数"
# 每次切换打一条脱敏日志（key 仅显示首尾若干字符）：
# multikey[kimi-coding]: 429 on alpha (sk-poo…0001), rotating to another key

# 未配置 pool 的 provider 回退 auth.json 单 key，行为不变
timeout 180 bin/gf pi --provider google -p "hi"
```

multikey.json 示例（`pools[].id` 匹配 provider，`enabled: false` 的 key 被过滤；
`cooldownMs` 为 429 冷却时长，`invalidKeyCooldownMs` 为 403/invalid 冷却时长）：

```json
{
  "pools": [
    {
      "id": "kimi-coding",
      "cooldownMs": 30000,
      "invalidKeyCooldownMs": 600000,
      "keys": [
        {"key": "sk-pool-alpha-0001", "label": "alpha", "enabled": true},
        {"key": "sk-pool-beta-00002", "label": "beta", "enabled": true}
      ]
    }
  ]
}
```

### 3. 插件生态与安装（`pi install`）

支持通过 `pi install` 命令一键安装社区或第三方的 Git 扩展仓库：

```bash
# 默认安装至全局扩展目录（~/.pi/agent/extensions/<name>），安装即生效
timeout 180 bin/gf pi install https://gitee.com/xxx/my-pi-extension.git

# 安装至当前项目局部目录（.pi/extensions/<name>）
timeout 180 bin/gf pi install --local https://gitee.com/xxx/my-project-extension.git
```

## 模型与 provider【可用】

```bash
# 默认走 settings.defaultProvider + settings.defaultModel（本机 google / gemini-3.8-flash）
timeout 300 bin/gf pi -p "hi"
```

同一 provider 切换不同模型（均已实测成功；完整模型清单见
`~/.pi/agent/models-store.json` 的 google.models）：

```bash
# flash 系列（不同版本）
timeout 120 bin/gf pi -p --model gemini-3.8-flash "只回复：ok"   # 默认模型
timeout 120 bin/gf pi -p --model gemini-3.5-flash "只回复：ok"   # ✅ 实测通过
timeout 240 bin/gf pi -p --model gemini-3.1-pro-preview "只回复：ok"  # ✅ pro 系列，实测通过

# 模型元数据解析（任务 034 起）：--model 必须在选定 provider 的目录中命中
# （models-store + models.json 合并目录），不匹配或查无此模型均报错退出码 1
# （不再 warning + fallback 兜底）
```

不同 provider 切换（均已实测成功）：

```bash
# google（Gemini，默认 provider；同样可显式指定）
timeout 300 bin/gf pi -p --provider google "只回复：google-ok"

# Anthropic（支持 ANTHROPIC_BASE_URL 兼容网关与 ANTHROPIC_AUTH_TOKEN 环境变量；
# 凭据缺失时明确报错退出码 1）
timeout 300 bin/gf pi -p --provider anthropic "只回复：anthropic-ok"
timeout 300 bin/gf pi -p --provider anthropic --tools bash "用 bash 运行 echo ok 并告诉我输出"

# kimi-coding（支持 auth.json 中 api_key 凭据与 KIMI_API_KEY 环境变量，已实测通过）
timeout 60 bin/gf pi --provider kimi-coding -p "请回复：kimi-ok"
timeout 120 bin/gf pi --provider kimi-coding --tools bash -p "用 bash 运行 echo kimi-ok 并告诉我输出"

# glm（智谱 GLM，基于 Anthropic 兼容网关，已实测通过）
timeout 60 bin/gf pi --provider glm -p "请回复：glm-ok"
timeout 120 bin/gf pi --provider glm --tools bash -p "用 bash 运行 echo glm-ok 并告诉我输出"

# deepseek（DeepSeek，基于兼容网关，已实测通过）
timeout 60 bin/gf pi --provider deepseek -p "请回复：deepseek-ok"
timeout 120 bin/gf pi --provider deepseek --tools bash -p "用 bash 运行 echo deepseek-ok 并告诉我输出"
```

注意：API 侧已对新增用户下线的模型会返回 404（如 gemini-2.5-pro），此时
stderr 输出人可读的 404 详情、退出码 1——这是正确的错误路径而非缺陷。

```bash
# 覆盖代理（socks5h:// 域名解析也走代理；http/https 代理亦可）
timeout 300 bin/gf pi -p --proxy socks5h://127.0.0.1:1080 "hi"
```

## 控制选项【可用】

```bash
# 轮数上限（默认 200，0 不限）；bash 工具与单次 HTTP 超时秒数（bash 默认取此值）
timeout 600 bin/gf pi -p --max-turns 20 --timeout 60 "遍历 goldfish/ 给出模块清单"

# bash 工具超时生效后模型可自主以更长 timeout 重试
timeout 300 bin/gf pi -p --tools bash --timeout 5 "用 bash 运行 sleep 30 然后告诉我结果"

# 会话级累计超时（默认 0，即不设上限；正数按需开启兜底）
timeout 7200 bin/gf pi -p --session-timeout 7000 "长任务执行（最多 7000 秒）"
bin/gf pi -p "长任务执行（默认不设会话超时）"

# 系统提示词：替换 preamble / 追加附加节（可重复）
timeout 300 bin/gf pi -p --system-prompt "You are a Scheme expert." "解释 call/cc"
timeout 300 bin/gf pi -p --append-system-prompt "回答末尾必须加上 [goldfish-ok]" "只回答两个字：好的"

# 帮助
bin/gf pi --help
```

## 模型列表【可用】（2026-10-19 验证）

```bash
# 列出当前 provider（默认）的可用模型（包含 provider、model、context、max-out、thinking、images 列）
timeout 30 bin/gf pi --list-models

# 指定 provider 过滤
timeout 30 bin/gf pi --provider google --list-models
timeout 30 bin/gf pi --provider kimi-coding --list-models

# 可选搜索过滤词（模糊/子串匹配）
timeout 30 bin/gf pi --list-models flash

# 列出全部已配置 provider 的可用模型
timeout 30 bin/gf pi --provider all --list-models
```

## 错误路径行为【可用】

```bash
# @文件不存在：stderr 人可读报错（含路径与原因），退出码 1
timeout 60 bin/gf pi -p @/nonexistent-file "hi"    # Error: cannot read @/nonexistent-file: No such file or directory

# 连接失败：stderr 人可读摘要，退出码 1
timeout 90 bin/gf pi -p --proxy socks5h://127.0.0.1:9 "hi"

# 模型不存在或不属于该 provider（任务 034 起）：配置层直接报错，退出码 1，不发 HTTP
timeout 120 bin/gf pi -p --model totally-fake-model "hi"
# → Error: Unknown model "totally-fake-model": not found in any configured provider model catalog...
```

## 函数文档索引【可用】

```bash
# 全量构建系统文档索引
timeout 120 bin/gf doc --build-json
# 若需检索 tools/pi 下的工具函数文档，带上 -I tools/pi：
timeout 120 bin/gf -I tools/pi doc --build-json
timeout 30 bin/gf -I tools/pi doc "pi-event-session" # 查看函数源码与测试
```

## 架构演进（目标态）

参照 pi 源码（`~/git/pi`）的三层包结构设计——`packages/ai`（provider 层）、
`packages/agent`（循环层）、`packages/coding-agent`（应用层）；goldfish 对齐为
`tools/pi/liii/` 下的多级子库（同 `(liii datetime xxx)` 先例）：

```
tools/pi/liii/
  pi.scm            # (liii pi)          门面：公开 API 再导出
  pi/
    cli.scm         # (liii pi cli)      CLI 入口（gfproject.json 注册指向）
    types.scm       # (liii pi types)    类型门面与再导出
    message.scm     # (liii pi message)  消息 record 与类型定义（message/record.scm, message/types.scm）
    event.scm       # (liii pi event)    事件 record 与类型定义（event/record.scm, event/types.scm）
    tools.scm       # (liii pi tools)    工具 record 与类型定义（tools/record.scm, tools/types.scm）
    config.scm      # (liii pi config)   配置 record 与类型定义（config/record.scm, config/types.scm）
    extension.scm   # (liii pi extension) 扩展机制门面（API、注册容器、生命周期分发）
    extension/      # 扩展机制子模块（api, registry, discovery, loader, hooks）
    extensions/     # ★ 内置轻量扩展（随包分发：web-search, simplify, code-review, multikey）
    models.scm      # (liii pi models)   模型目录解析与表格输出（models/list.scm）
    gemini.scm      # (liii pi gemini)   Gemini provider（gemini/core, request, stream, stream-parse）
    anthropic.scm   # (liii pi anthropic) Anthropic provider（anthropic/core, request, stream, stream-parse）
    kimi.scm        # (liii pi kimi)     Kimi provider（kimi/core, msg, request, stream, stream-parse）
    deepseek.scm    # (liii pi deepseek) DeepSeek provider（兼容网关路径与 OpenAI 协议）
    provider.scm    # (liii pi provider) Provider 统一分发调度（含 glm 路由至 anthropic 链路）
    auth.scm        # (liii pi auth)     凭据解析（auth.json + 环境变量 fallback）
    retry.scm       # (liii pi retry)    指数退避重试（≈ ai/src/utils/retry）
    agent.scm       # (liii pi agent)    Agent 循环与事件发射（≈ packages/agent）
    prompt.scm      # (liii pi prompt)   系统提示词与项目上下文（≈ core/system-prompt.ts）
    session.scm     # (liii pi session)  会话持久化与恢复（≈ core/session-manager.ts）
    args.scm        # (liii pi args)     参数解析与输入组装（≈ cli/）
    modes.scm       # (liii pi modes)    print/json 输出模式（≈ modes/print-mode + json-event）
    read.scm        # (liii pi read)     read 工具
    write.scm       # (liii pi write)    write 工具
    edit.scm        # (liii pi edit)     edit 工具
    bash.scm        # (liii pi bash)     bash 工具
    grep.scm        # (liii pi grep)     grep 工具
    find.scm        # (liii pi find)     find 工具
    ls.scm          # (liii pi ls)       ls 工具
    truncate.scm    # (liii pi truncate) 输出截断
    help.scm        # (liii pi help)     帮助信息
    time.scm        # (liii pi time)     时间辅助
    template.scm    # (liii pi template) 模板辅助
```

**无平铺族**：`(liii pi-xxx)` 形式的模块一律不存在——CLI 入口也是多级子库
`(liii pi cli)`，所有代码只以 `(liii pi)` 与 `(liii pi xxx)` 两种形式组织。

**分层与拆分规则**：二级子库（如 `(liii pi gemini)`）是**导出函数的层面**——
对外公开该域的 API；单文件 300 行放不下就把实现继续拆到下一层子文件
（如 `pi/gemini/request.scm`、`pi/gemini/stream.scm`，由 `(liii pi gemini)` 统一
导出），与 `bin/gf check` 的 300 行硬约束一致。

**函数文档放哪**：goldfish 的函数文档 = 测试文件本身（`<函数名>-test.scm`，
`gf doc` 从中提取源码与用法）。测试目录镜像源码库结构——`(liii pi gemini)`
的函数测试在 `tools/pi/tests/liii/pi/gemini/<函数名>-test.scm`，更深层子文件
（`pi/gemini/request.scm`）的在其同名子目录下，以此类推。

进度（2026-10-19）：
- ✅ 任务 001：文件物理归口 `tools/pi/liii/`
- ✅ 任务 002：测试迁至 `tools/pi/tests/liii/pi/`，doc 索引重建
- ✅ 任务 003：模块名与目录统一，彻底消除平铺模块，二级子库全部落位
- ✅ 任务 004-007：事件、消息、配置、工具全面 record 化（保持外部兼容）
- ✅ 任务 008：Kimi provider 完整支持（非流式、流式、工具调用与错误处理）
- ✅ 任务 015：record 化收尾全量真实冒烟通过
- ✅ 任务 017：测试去除对计划文档（Agents.md）的硬依赖
- ✅ 任务 018：`--list-models` 命令支持与对齐格式化输出
- ✅ 任务 019：GLM provider 完整支持（复用 Anthropic 兼容网关链路）
- ✅ 任务 020：DeepSeek provider 完整支持（独立子库与兼容网关支持）
- ✅ 任务 021：PR 1141 CI 跨平台全绿门禁修复（macOS/Linux/Windows）

## TODO（尚不能运行，随迭代补充）

- [x] **GLM 5.3 系列接入【可用】（任务 028 已验收）**：重点支持 GLM 5.3 系列，具备 100 万超长上下文窗口并原生支持思考模式与工具调用。推荐模型：`glm-5.3`（1M 上下文旗舰主力）、`glm-5.3-flash`（1M 上下文轻量高速版）、`glm-5-turbo`（200K 上下文日常开发）、`glm-4.5-air`。已实测用法：

  ```bash
  # 使用 GLM-5.3 主力模型提问（实测 2026-10-19，退出码 0）
  timeout 120 bin/gf pi --provider glm --model glm-5.3 -p "用一句话解释 Scheme 的 continuation"

  # 使用 GLM-5.3-Flash 高速模型执行 Bash 工具任务（实测工具调用）
  timeout 180 bin/gf pi --provider glm --model glm-5.3-flash --tools bash -p "统计 tests/ 目录下的测试用例总数"

  # 列出全部 glm 模型（6 条：5.3/5.3-flash/5-turbo/4.5-air/4.5/4-flash）
  bin/gf pi --provider glm --list-models
  ```

  注：glm 模型目录来自 `~/.pi/agent/models.json` 用户自定义目录（任务 029 的合并
  逻辑零改动覆盖）；验收脚本 `tools/pi/devel/028.scm` 41/41 全绿。
- [x] **DeepSeek v4 系列接入【可用】（任务 029 已验收）**：经由 `https://api.deepseek.com/anthropic` 端点接入。推荐模型：`deepseek-v4-flash`（1M 上下文旗舰版，支持深度思考链与代码工具调用）、`deepseek-chat`（通用对话）、`deepseek-reasoner`（强化推理版）。已实测用法：

  ```bash
  # 使用 DeepSeek V4 Flash 进行代码问答（实测 2026-10-19，退出码 0）
  timeout 120 bin/gf pi --provider deepseek --model deepseek-v4-flash -p "解释尾递归优化的原理"

  # 使用 DeepSeek V4 Flash 配合 Bash 工具审查变更（实测管道+工具调用）
  git diff | timeout 180 bin/gf pi --provider deepseek --model deepseek-v4-flash --tools bash -p "审查当前 git diff 并提出改进建议"

  # 列出全部 deepseek 模型（5 条：chat/flash/reasoner/v4-flash/v4-pro）
  bin/gf pi --provider deepseek --list-models
  ```

  注：模型目录来自 `~/.pi/agent/models.json` 用户自定义目录（任务 029 起合并进 models-store）。
- [x] **MiniMax 中国大陆版接入【可用】（任务 030 已验收）**：复用 auth.json 中的 `sk-cp-...` 凭据，接入 `https://api.minimaxi.com/anthropic`（国际站 `api.minimax.io` 自动归一）。模型：`MiniMax-M3.1-Flash`（1M 上下文、524K 最大输出，**轮换池主力**——特别快）、`MiniMax-M3`（1M/512K 多模态旗舰）、`MiniMax-M2.7`（204.8K）与 `MiniMax-M2.7-highspeed`（高速版）。已实测用法：

  ```bash
  # 使用 MiniMax-M3.1-Flash 主力模型提问（实测 2026-10-19，秒回）
  timeout 120 bin/gf pi --provider minimax --model MiniMax-M3.1-Flash -p "用 Scheme 实现快速排序"

  # 使用 MiniMax-M3 调用 Bash 与读写工具处理实际任务（实测工具调用）
  timeout 180 bin/gf pi --provider minimax --model MiniMax-M3 --tools read,bash,write -p "运行 git status 并总结当前工作区状态"

  # 大陆站别名 provider 与模型目录
  bin/gf pi --provider minimax-cn --list-models
  ```

  注：minimax 与 minimax-cn 双 provider 均可用；环境变量 MINIMAX_API_KEY /
  MINIMAX_BASE_URL 可覆盖（任务 035 env 优先体系）。
- [x] **`pi-multikey` 密钥池与 429/403 自动轮换【可用】（任务 033，已扩展化）**：
  对照 Node pi 的 `pi-multikey` 扩展移植。**已剥离为内置扩展**（`extensions/multikey.scm`，
  实现库族 `(liii pi multikey pool/load/policy/config)`；核心只保留通用 key 钩子接口 `(liii pi ai keyring)`），
  用法与配置详见「常用内置扩展」章节。
- [ ] **4 字节 UTF-8 + '<' 挂起**：prompt 同时含 4 字节 UTF-8 字符（如 🚀、𠀀）与 `<` 时进程静默挂起且连 HTTP 300s 超时都不生效（复现：`timeout 60 bin/gf pi -p '🚀 <'`）。【待重做】
- [ ] **thinking 内容**：思考级别的请求与解析。预期用法（实现时以 pi 实际 CLI 为准）：

  ```bash
  # 指定思考级别（off / minimal / low / medium / high / xhigh / max；未指定时取 settings.defaultThinkingLevel）
  timeout 300 bin/gf pi -p --thinking high "分析这段递归的时间复杂度并优化"
  # JSON 事件流中出现 thinking 增量块（assistantMessageEvent type 区分 text/thinking）
  timeout 300 bin/gf pi --mode json --thinking low "证明：任意三角形内角和为180度" > t.jsonl
  ```

  实现要点：请求侧 Gemini 走 `generationConfig.thinkingConfig`（含 `includeThoughts`），
  Anthropic 走 thinking block；响应侧 thinking part 解析进消息结构但 print 模式
  默认只输出 text 块。
- [ ] **图片输入**：`@文件` 扩展支持图片。预期用法：

  ```bash
  # @路径指向图片时作为图像 part 注入（而非文本 file block）
  timeout 300 bin/gf pi -p @screenshot.png "这张截图里的报错是什么？"
  timeout 300 bin/gf pi -p "对比这两个界面" @ui-v1.png @ui-v2.png
  ```

  实现要点：按扩展名/MIME 判断图片类型，Gemini 走 `inlineData` part（base64），
  Anthropic 走 `image` source block；不支持的格式明确报错。
- [ ] **交互式 TUI 终端模式（第二阶段重点任务）**：基于刚刚合入主干的通用终端 UI 库 `(liii tui)`（PR #1159），实现与原生 pi 深度对齐的原生交互模式。预期用法：

  ```bash
  # 缺省无参时自动进入基于 (liii tui) 的交互终端界面
  bin/gf pi

  # 指定 provider / model 进入交互会话
  bin/gf pi --provider google --model gemini-3.8-flash
  ```

  实现要点：
  - 核心事件循环与 `(liii tui terminal)` 非阻塞按键轮询（081/082）；
  - 底部多行 Prompt 输入框（集成 `(liii tui editor)`，支持单行/多行编辑、光标移动、历史记录上下翻阅）（083）；
  - 响应式多分区视口与窗口 Resize 动态自适应（084）；
  - 历史消息自然折行排版（全面支持 CJK 汉字与 Emoji 2 列视觉显示宽度）与流式 SSE 增量文本行差分无闪烁实时打字（085/086）；
  - Thinking 深度思考过程折叠/展开组件（087）；
  - 工具调用动态看板（带多帧动画 Spinner 与原地完成态覆盖）（088）；
  - Slash 命令悬浮弹出与自动补全菜单（089）；
  - 人在回路敏感工具调用（write/edit/bash）模态交互确认弹窗（090）；
  - 全局快捷键（Ctrl+C 中断、Ctrl+L 清屏、/help 手册）（091）；
  - 基于虚拟终端 `(make-virtual-terminal)` 的 100% 确定性自动化 CI 回归测试（092）。

不做项：
- RPC 模式（IDE 跨进程 JSONL over stdio，非当前核心需求）
- 项目级 `.pi/settings.json` 读取（首期只读全局 settings.json，暂不引入 trust 门控概念）
## web_search 工具【可用】（任务 040-043 四阶段落地；2026-10-20 扩展化并默认启用）

基于 Gemini 原生 Search Grounding（或 DeepSeek Search/Fetch API），与 pi 本尊
pi-web-search 扩展行为对齐。**已剥离为内置扩展**（`extensions/web-search.scm`，
实现库 `(liii pi web-search)`）且**默认启用**：无需 `--tools` 指定，
`--exclude-tools web_search` 可关闭；后端切换与配置详见「常用内置扩展」章节。

```bash
# 默认已启用，直接搜索（无需 --tools web_search）
timeout 280 bin/gf pi -p "搜索 Goldfish Scheme 最新版本并给出引用来源"

# JSON 模式事件：tool_execution_start（query/urls）→ tool_execution_end（sources/details）
timeout 280 bin/gf pi --mode json -p "..."
```

实现要点：googleSearch 节注入（幂等/与 functionDeclarations 共存）、groundingMetadata
三要素解析（webSearchQueries/groundingChunks/groundingSupports，对齐 extractGoogleSearchDetails）、
Sources/Additional Results 拼装、7 类错误路径（401/403/超时/非 JSON/空 candidates 等）。


## bin/gf pi 与 pi 的差距（自举差距清单）

> 本节由自举试验的差距分析维护：每次 `bin/gf pi` 尝试执行真实任务失败时，
> 分析它与 pi（node 版）的差距并登记在此，作为 `bin/gf pi` 改进的规划依据。

| # | 差距 | 证据（失败的任务/场景） | 状态 |
|---|---|---|---|
| 1 | ~~4 字节 UTF-8 字符与 `<` 组合导致请求挂起~~ | 阶段五冒烟 `'🚀 <'`（pi 正常处理同类 prompt） | ✅ 已修复（任务 014：会话级超时兜底） |
| 2 | 无 `--mode rpc`（pi 有 JSONL over stdio 的 RPC 模式，IDE/长会话集成需要） | pi docs/rpc.md；bin/gf pi 仅 print/json | 未规划（Non-goal 暂缓） |
| 3 | OAuth 类凭据不消费（pi 支持 oauth 轮换） | auth.json 的 oauth 条目 | 不做（用户裁定：维护费劲；当前各 provider 均用 api_key/环境变量） |
| 4 | thinking 内容不请求不解析 | `--thinking` 用法在 TODO | 远期（任务 012） |
| 5 | 图片输入不支持 | `@图片` 用法在 TODO | 不做（用户裁定 2026-10-19：任务 013 取消） |
| 6 | `--model` 不跨 provider 自动路由：bin/gf pi 只在当前 provider（--provider 或 defaultProvider）内解析模型 ID；pi 的 model-resolver 会在全部 provider 模型目录中搜索 ID，命中即隐式选定 provider（如 `pi -p --model k3` 自动路由到 kimi） | 实测：`bin/gf pi -p --model k3` 把 k3 发给 Google 得 HTTP 404，而 pi 正常路由 | ✅ 已修复（任务 022） |
| 7 | Windows 下 bash 工具挂死：子进程 spawn 方式在 Windows 不可用，bash 工具执行任何命令均挂到超时（纯文本对话正常）；bin/gf pi 因此无法跑 git/测试命令，是自举（CLAUDE §5.5）的前置缺陷 | 实测（2026-10-19）：`timeout 180 bin/gf pi --mode json --tools bash "用 bash 工具运行命令 echo win-bash-ok"` → tool_execution_end "Command timed out after 120 seconds" isError:true | 修复中（任务 026，优先） |
| 8 | 会话级超时兜底（任务 014，默认 600s）无法从 CLI 放宽：args.scm 无 `--session-timeout` 选项，`--timeout` 只管 bash/HTTP；自举执行大体量开发任务必在 ~10 分钟被 `Session timeout exceeded` 打断（pi 无此硬上限）。注：012/022 曾误判为 `--timeout 7000` 解决，实为任务恰好在 600s 内完成 | 实测（2026-10-19）：任务 025/031 开发阶段自举均运行 ~10 分钟被会话兜底终止 | ✅ 已修复（任务 031：`--session-timeout <秒>`，0 关闭兜底） |
| 9 | ~~不消费 settings.json 的 `retry.provider` 嵌套节~~ | 可重试 provider 错误（429/5xx/408/409/网络错误）现使用 `retry.provider.maxRetries` 预算（本机=12），未配置嵌套节时回退 `retry.maxRetries`（缺省 3），与 pi 的 SDK maxRetries 语义对齐 | ✅ 已修复（任务 032：TDD 覆盖嵌套解析/缺省回退/stub server 429 预算窗口 3.5b/3.5c） |
| 10 | `KIMI_API_KEY` 环境变量兜底未生效：auth 解析只读 auth.json 的 kimi-coding 条目，env 被忽略——假 key 也返回周配额 403（而非 401），证明请求从未携带 env key；阻塞三把 kimi key 分摊配额 | 实测（2026-10-19）：`KIMI_API_KEY=sk-bogus bin/gf pi --provider kimi-coding --model k3` 返回周配额 403；**对照**：同 hongwei key `KIMI_API_KEY=... pi --provider kimi-coding --model k3` 本尊返回 ok（pi 源码 env-api-keys.ts 同名变量、env 优先） | ✅ 已修复（任务 035：env > auth.json 统一五个 provider；修复后假 key 实测返回 401；验收脚本 035.scm 27/27） |
| 11 | deepseek 首轮 tool_use 参数解析崩溃：`Error: (value-error (("input") " must be null, pair, or list with at least 2 elements"))`——会话记录仅 2 行（session+user），崩溃发生在首轮 API 响应解析（未执行任何工具）；037 开发两次拉起同点确定性复现（030/033/036 用 deepseek 未触发，疑与 deepseek 某类 tool_use 参数形状相关，如 input 为 JSON 字符串/嵌套形状） | 实测（2026-10-20）：037 开发第 1/2 次拉起均在加载 Agents.md 首轮响应后秒崩，会话 jsonl 取证 | ✅ 已修复（任务 050：根因 C++ json_write_object_entry 对单元素 list 约束 + 空参数 cons 退化；anthropic-normalize-input/parse-tool-arguments 多形态兼容；deepseek 工具调用实测恢复） |
| 12 | deepseek 流式间歇性 `HTTP 400 stream error`：验收阶段大上下文拉起偶发连续中断（非每次——039 文档/开发、040 文档/开发均 deepseek 成功；039 验收×2 与 040 验收×1 连续遇到）——疑 deepseek 网关对某类大 prompt 流式响应的偶发拒绝；未定位到请求体差异 | 实测（2026-10-20）：三次同错（/tmp/pi-goldfish-run.log 取证），换模型即恢复 | 观察中（复现频繁时登记修复任务；当前以换模型规避） |
| 13 | goldfish2 快照同步盲区：任务改动**主库 goldfish/liii/** 且未 commit 时，git 同步拿不到工作区改动，执行器（goldfish2 bin/gf）加载工作区 pi 代码报 `unbound variable ~S in ~S`（052 改 date-time 主库两次触发）；修复：同步时 diff 主库并拷贝未提交文件 | 实测（2026-10-20）：052 验收两次秒崩，cp date-time.scm 后恢复；附带发现错误消息 ~S 占位符未替换的 cosmetic bug（观察中） |

## Provider 支持范围

目前已支持五大 Provider：
- **google（Gemini）**：官方原生 API（流式 SSE、function calling、思考能力预留）
- **kimi-coding（Kimi）**：Moonshot AI 官方编码模型（OpenAI Chat Completions 协议兼容）
- **deepseek（DeepSeek）**：DeepSeek 官方模型（基于兼容网关路径与 OpenAI 协议）
- **glm（智谱 GLM）**：智谱大模型（基于 Anthropic Messages 兼容网关）

**OAuth 不做（用户裁定）**：OAuth 类凭据（access/refresh 轮换、token 过期刷新）
整体不做——维护起来比较费劲（各家刷新端点与协议不同、token 生命周期管理持续
消耗精力）。凭据一律走 `type:"api_key"` 或环境变量；auth.json 中 `type:"oauth"`
的条目不消费。Anthropic 官方 OAuth 同理不做（本机无官方访问权限，其兼容网关
能力已被 glm 复用）。
- **anthropic（Anthropic）**：Anthropic 协议与兼容网关（凭据缺失明确报错，支持环境变量配置）

## 开源协议与第三方版权归属（License & Attribution）

`tools/pi`（`bin/gf pi`）及其相关测试用例采用 [MIT License](LICENSE) 授权开源。

`tools/pi`（`bin/gf pi`）及支撑库 `(liii pi)` 的核心架构、控制流、提示词工程与扩展模式，主要由 Node.js / TypeScript 生态中的 upstream pi 核心仓库及周边开源插件移植（Porting）而来。在此特别鸣谢以下开源原作者及项目贡献者的卓越工作：
- **Mario Ortiz Manero / Earendil Works**：pi 核心库 [`@earendil-works/pi-coding-agent`](https://github.com/earendil-works/pi)
- **Matt Devy**：pi 官方周边扩展生态 [`MattDevy/pi-extensions`](https://github.com/MattDevy/pi-extensions)（涵盖 `pi-code-review`、`pi-simplify`、`pi-red-green`、`pi-blueprint` 与 `pi-compass`）
- **kslamph**：多 Key 轮换插件 [`pi-multikey`](https://github.com/kslamph/multikey)
- **ttttmr**：联网搜索插件 [`pi-web-search`](https://github.com/ttttmr/pi-web-search)

以上上游项目均基于宽松的 MIT 许可证发布。本项目作为跨语言演绎作品，完整保留了原作者的版权声明与许可证说明。详细的版权清单与许可证全文，请参阅 [`tools/pi/LICENSE`](LICENSE)。

