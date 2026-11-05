# (liii pi code modes config json-write) 测试导览

`(liii pi code modes config json-write)` 是配置 TUI 的写盘适配层，
底层复用 `(liii pi code config json-write)`。

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/modes/config/json-write/
```

## 测试文件索引

- `pi-config-apply-writes-test.scm` — 配置写入计划落盘

注：`pi-config-json-pretty` 为纯转发导出，其单元测试位于实现库
`tests/liii/pi/code/config/json-write/pi-config-json-pretty-test.scm`。
