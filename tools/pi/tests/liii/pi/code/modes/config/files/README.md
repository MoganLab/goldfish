# (liii pi code modes config files) 测试导览

`(liii pi code modes config files)` 提供配置 TUI 的只读文件视图、启停计划与
默认模型设置计划。

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/modes/config/files/
```

## 测试文件索引

- `pi-config-file-view-test.scm` — provider/模型配置文件只读视图
- `pi-config-plan-toggle-test.scm` — provider/模型启停写入计划
- `pi-config-plan-set-default-test.scm` — 默认模型设置写入计划
