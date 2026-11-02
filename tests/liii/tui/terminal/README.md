# (liii tui terminal) 终端抽象层测试导览

`(liii tui terminal)` 是 TUI 框架的终端抽象层：以 terminal 记录类型统一
标准终端（stdout + ANSI 转义序列）与虚拟终端（测试用内存实现），提供
尺寸探测、按键轮询、光标控制、清屏、同步更新、raw 模式、备用屏、
鼠标与粘贴模式等能力。

## 模块组成

- `goldfish/liii/tui/terminal/protocol.scm` — terminal 记录类型与通用操作
  （`make-terminal-record` / `terminal?` / `terminal-size` / `terminal-write` /
  光标与清屏 / raw 模式 / 备用屏 / 鼠标与粘贴模式）
- `goldfish/liii/tui/terminal/size.scm` — `terminal-detect-size`（TIOCGWINSZ）
- `goldfish/liii/tui/terminal/standard.scm` — `make-standard-terminal`
  （stdout 实现，支持 `:inline` 相对定位模式）
- `goldfish/liii/tui/terminal/virtual.scm` — `make-virtual-terminal`
  （内存虚拟终端：screen / raw 输出流 / 按键队列 / resize）
- `goldfish/liii/tui/terminal/poll.scm` — `terminal-poll-key` 与
  ANSI 转义序列、鼠标事件解析

## 运行测试

```bash
bin/gf tests/liii/tui/terminal/poll-test.scm
bin/gf tests/liii/tui/terminal/make-virtual-terminal-test.scm
bin/gf tests/liii/tui/terminal/terminal-write-test.scm
```

目录下其余测试文件同样以 `bin/gf tests/liii/tui/terminal/<文件名>` 逐个运行。

## 测试文件索引

- `poll-test.scm` — `terminal-poll-key`、粘贴模式开关、UTF-8 多字节与
  转义序列解析（含不完整 CSI/SS3 序列返回 #f 的回归）
- `make-standard-terminal-test.scm` — 标准终端构造
- `make-virtual-terminal-test.scm` — 虚拟终端构造
- `standard-terminal-inline-test.scm` — `:inline` 相对定位模式
- `terminal-alt-screen-test.scm` — 备用屏进入/退出
- `terminal-begin-sync-test.scm` — 同步更新开始
- `terminal-clear-line-to-end-test.scm` — 清行至行尾
- `terminal-cursor-test.scm` — 光标查询与定位、清屏、显隐光标
- `terminal-detect-size-test.scm` — 终端尺寸探测
- `terminal-drain-mouse-events-test.scm` — 鼠标事件合并排空
- `terminal-end-sync-test.scm` — 同步更新结束
- `terminal-write-test.scm` — 文本写入
- `virtual-terminal-resize-test.scm` — 虚拟终端缩放

## 存量整改说明

goldcheck `unit-test` 规则要求按导出函数逐个拆分
`<函数名>-test.scm` 测试文件（`!` → `-bang`、`?` → `-p`）。本目录现有
测试多为按主题聚合的综合性文件，按函数拆分的补齐属存量整改任务，
另行跟进，不随单个小修 PR 携带。
