# (liii pi code modes config json-write) 测试导览

`(liii pi code modes config json-write)` 是配置 TUI 的写盘适配层，
底层复用 `(liii pi code config json-write)`。

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/modes/config/json-write/
```

## 测试文件索引

- `pi-config-json-pretty-test.scm` — JSON 序列化
- `pi-config-apply-writes-test.scm` — 配置写入计划落盘
