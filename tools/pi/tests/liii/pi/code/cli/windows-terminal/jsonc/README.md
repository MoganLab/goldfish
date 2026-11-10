# (liii pi code cli windows-terminal jsonc) 测试导览

验证 Windows Terminal JSONC 配置的 Alt+Enter 状态识别和局部 unbound 插入，
覆盖注释、尾逗号、旧版 keybindings、自定义键位与幂等性。

```bash
bin/gf test tools/pi/tests/liii/pi/code/cli/windows-terminal/jsonc/
```

## 测试文件索引

- `wt-detect-alt-enter-status-test.scm` — Alt+Enter 绑定状态与异常输入
- `jsonc-ws-p-test.scm` — 空白字符分类
- `jsonc-skip-comment-at-test.scm` — 行注释与块注释跳转
- `jsonc-skip-ws-and-comments-test.scm` — 连续空白与注释跳过
- `jsonc-next-str-state-test.scm` — 字符串与转义状态迁移
