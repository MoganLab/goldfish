# PI_SPEC：`bin/gf pi` 命令行使用说明

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
# 默认只激活 read/bash/edit/write；grep/find/ls 需显式开启
timeout 300 bin/gf pi -p --tools read,bash,grep,find,ls "统计 tests/ 下测试文件数量"

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

# 纯内存运行，不写盘
timeout 300 bin/gf pi --no-session -p "一次性问题"
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

# 模型元数据解析顺序：--model 精确匹配 models-store 条目，取不到则用兜底值
# （contextWindow=128000 / maxTokens=16384）并向 stderr 发 warning
```

不同 provider 切换（均已实测成功）：

```bash
# google（Gemini，默认 provider；同样可显式指定）
timeout 300 bin/gf pi -p --provider google "只回复：google-ok"

# Anthropic（支持 ANTHROPIC_BASE_URL 兼容网关与 ANTHROPIC_AUTH_TOKEN 环境变量；
# 凭据缺失时明确报错退出码 1）
timeout 300 bin/gf pi -p --provider anthropic "只回复：anthropic-ok"
timeout 300 bin/gf pi -p --provider anthropic --tools bash "用 bash 运行 echo ok 并告诉我输出"

# kimi-coding（TODO，见文末）
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

# 系统提示词：替换 preamble / 追加附加节（可重复）
timeout 300 bin/gf pi -p --system-prompt "You are a Scheme expert." "解释 call/cc"
timeout 300 bin/gf pi -p --append-system-prompt "回答末尾必须加上 [goldfish-ok]" "只回答两个字：好的"

# 帮助
bin/gf pi --help
```

## 错误路径行为【可用】

```bash
# @文件不存在：stderr 人可读报错（含路径与原因），退出码 1
timeout 60 bin/gf pi -p @/nonexistent-file "hi"    # Error: cannot read @/nonexistent-file: No such file or directory

# 连接失败：stderr 人可读摘要，退出码 1
timeout 90 bin/gf pi -p --proxy socks5h://127.0.0.1:9 "hi"

# 模型不存在：stderr 输出 API 404 详情，退出码 1
timeout 120 bin/gf pi -p --model totally-fake-model "hi"
```

## 函数文档索引【可用】

```bash
# (liii pi) 测试按"一个函数一个测试文件"组织（tests/liii/pi/<函数名>-test.scm）
timeout 120 bin/gf doc --build-json     # 重建索引
timeout 30 bin/gf doc "pi-event-session" # 查看函数源码与测试
```

## 架构演进（目标态）

参照 pi 源码（`~/git/pi`）的三层包结构设计——`packages/ai`（provider 层）、
`packages/agent`（循环层）、`packages/coding-agent`（应用层）；goldfish 对齐为
`tools/pi/liii/` 下的多级子库（同 `(liii datetime xxx)` 先例）：

```
tools/pi/liii/
  pi.scm            # (liii pi)          门面：公开 API 再导出
  pi/
    types.scm       # (liii pi types)    消息/usage/事件类型与构造（≈ ai/src/types.ts、agent/types.ts）
    gemini.scm      # (liii pi gemini)   Gemini provider：请求构造/SSE/映射（≈ ai/src/api/google-*）
    anthropic.scm   # (liii pi anthropic) Anthropic provider（≈ ai/src/api/anthropic-messages）
    auth.scm        # (liii pi auth)     凭据解析（≈ ai/src/auth + core/auth-storage）
    models.scm      # (liii pi models)   模型目录与元数据（≈ ai/src/models-store + model-catalog）
    retry.scm       # (liii pi retry)    指数退避重试（≈ ai/src/utils/retry）
    agent.scm       # (liii pi agent)    agent 循环与事件发射（≈ packages/agent）
    tools.scm       # (liii pi tools)    七工具 + 截断 + 路径（≈ core/tools/）
    config.scm      # (liii pi config)   settings/auth/models-store 只读读取（≈ core/config.ts）
    prompt.scm      # (liii pi prompt)   系统提示词与项目上下文（≈ core/system-prompt.ts）
    session.scm     # (liii pi session)  会话持久化与恢复（≈ core/session-manager.ts）
    args.scm        # (liii pi args)     参数解析与输入组装（≈ cli/）
    modes.scm       # (liii pi modes)    print/json 输出模式（≈ modes/print-mode + json-event）
  pi-cli.scm        # (liii pi-cli)      CLI 入口（≈ main.ts；gfproject.json 注册指向）
```

**分层与拆分规则**：二级子库（如 `(liii pi gemini)`）是**导出函数的层面**——
对外公开该域的 API；单文件 300 行放不下就把实现继续拆到下一层子文件
（如 `pi/gemini/request.scm`、`pi/gemini/sse.scm`，由 `(liii pi gemini)` 统一
导出），与 `bin/gf check` 的 300 行硬约束一致。上面的子库清单只是例子，
实际划分按代码自然边界走。

进度：文件迁移（任务 001）已完成——物理位置已归口 `tools/pi/liii/`；多级子库
重组（任务 003）待执行，当前 `(liii pi)` 内部仍为平铺子模块，`(liii pi-cli)`
为 CLI 入口。

## TODO（尚不能运行，随迭代补充）

- [ ] **Kimi provider**：`~/.pi/agent/auth.json` 中已配置的 `kimi-coding`（type:"api_key"，仅 key/type 字段）尚未被消费；需参照 pi 源码（`~/git/pi`）中 Kimi 的端点与鉴权实现，支持 `--provider kimi-coding` 端到端。不涉及 OAuth。【进行中】
- [ ] **4 字节 UTF-8 + '<' 挂起**：prompt 同时含 4 字节 UTF-8 字符（如 🚀、𠀀）与 `<` 时进程静默挂起且连 HTTP 300s 超时都不生效（复现：`timeout 60 bin/gf pi -p '🚀 <'`）。【修复中】
- [ ] **thinking 内容**：思考级别的请求与解析。预期用法（实现时以 pi 实际 CLI 为准）：

  ```bash
  # 指定思考级别（off / low / medium / high；未指定时取 settings.defaultThinkingLevel）
  timeout 300 bin/gf pi -p --thinking-level high "分析这段递归的时间复杂度并优化"
  # JSON 事件流中出现 thinking 增量块（assistantMessageEvent type 区分 text/thinking）
  timeout 300 bin/gf pi --mode json --thinking-level low "证明：任意三角形内角和为180度" > t.jsonl
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
不做项：
- 交互式 TUI / RPC 模式（goldfish 缺少相关的终端 UI 基础设施，不规划）
- 项目级 `.pi/settings.json` 读取（首期只读全局 settings.json，暂不引入 trust 门控概念）

## Provider 支持范围

当前仅支持两个 provider：**google（Gemini）** 与 **kimi-coding（Kimi，TODO 中）**。
Anthropic 虽已实现，但本机无 Anthropic 访问权限，不做 OAuth 凭据过期自动刷新，
该路径不在维护范围内。
