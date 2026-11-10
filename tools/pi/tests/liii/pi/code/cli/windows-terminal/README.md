# (liii pi code cli windows-terminal) 测试导览

覆盖 Windows Terminal 的 Alt+Enter 默认全屏绑定检测、用户确认、JSONC 局部修改、
幂等写回与失败回退，以及非 Windows、非 TTY 和 headless 路径的零副作用要求。

## 模块组成

- `tools/pi/liii/pi/code/cli/windows-terminal.scm` — 交互入口编排与配置安全写回
- `tools/pi/liii/pi/code/cli/windows-terminal/jsonc.scm` — JSONC 状态检测与局部插入

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/cli/windows-terminal/
```

## 测试文件索引

- `pi-windows-terminal-adapt-interactive-bang-test.scm` — 确认、拒绝、幂等、失败回退与零副作用
- `wt-find-settings-path-test.scm` — 正式版、Preview 与非打包安装路径的查找优先级
- `jsonc/wt-detect-alt-enter-status-test.scm` — 默认冲突、已解绑、自定义键位及 JSONC 状态识别
- `insert/wt-insert-unbound-action-test.scm` — 注释保留、局部插入与重复执行幂等
