# (liii pi code modes config) 配置 TUI 测试导览

`gf pi config` 的 ranger 风格三列配置浏览 TUI：左列 provider 白名单、
中列选中 provider 的模型、右列模型详情预览；支持方向键/j/k 导航、
空格启停、Enter 设默认模型、v 只读查看配置文件、q/Esc/Ctrl+C 退出。
测试使用 virtual-terminal 驱动完整主循环，无需真实终端。

## 模块组成

- `tools/pi/liii/pi/code/modes/config/app.scm` — `pi-config-run` 主循环与
  `pi-config-key-handler` 按键处理
- `tools/pi/liii/pi/code/modes/config/layout.scm` — `pi-config-render`
  三列纯文本渲染（行数严格守恒）
- `tools/pi/liii/pi/code/modes/config/overlay.scm` —
  `pi-config-render-overlay` / `pi-config-overlay-step` 只读覆盖层
- `tools/pi/liii/pi/code/modes/config/state.scm` — `pi-config-state`
  只读视图模型（目录装配）
- `tools/pi/liii/pi/code/modes/config/files.scm` — 启停/设默认的写入计划
  与落盘

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/modes/config/app-test.scm
bin/gf test tools/pi/tests/liii/pi/code/modes/config/layout-test.scm
bin/gf test tools/pi/tests/liii/pi/code/modes/config/overlay-test.scm
bin/gf test tools/pi/tests/liii/pi/code/modes/config/files-test.scm
bin/gf test tools/pi/tests/liii/pi/code/modes/config/state-test.scm
```

## 测试文件索引

- `app-test.scm` — `pi-config-run`：导航、启停、设默认、覆盖层、
  缩窗自适应与最后一个 provider 模型列完整导航（off-by-one 回归）
- `layout-test.scm` — `pi-config-render`：列宽、三列内容、行数与宽度
  约束、极端高度行数守恒、选中行高亮
- `overlay-test.scm` — `pi-config-render-overlay` / `pi-config-overlay-step`：
  覆盖层渲染、极端高度行数守恒、覆盖层按键交互
- `files-test.scm` — 启停与设默认的写入计划、落盘与级联
- `state-test.scm` — `pi-config-state`：目录装配、默认项定位、计数
