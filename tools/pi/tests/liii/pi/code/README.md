# (liii pi code) 代码组织导览

`tools/pi/liii/pi/code/` 根目录只保留 `cli`、`config`、`modes` 等入口库，
其余实现归入 `cli/`、`config/`、`core/`、`modes/`、`utils/` 等子目录
（对齐上游 packages/coding-agent 的 src/cli、src/core、src/modes、src/utils 组织）。

## 子目录导览

- `cli/` - 命令行参数解析与帮助文本
- `config/` - 用户配置目录读取与 settings.json 读写
- `core/` - 压缩、扩展机制、子进程失败策略、提示词、会话与工具
- `modes/` - 交互式 TUI、print/json 模式与配置 TUI
- `utils/` - Git 变更收集与 shell 输出处理
- `session-state/` - 恢复会话时的投影状态合并

## 结构约束

`library-structure-test.scm` 逐一导入各库路径并验证代表性导出，防止结构回退。
