# (liii pi code config json-write) 测试导览

`(liii pi code config json-write)` 提供用户配置目录的 JSON 序列化与原子写盘能力。

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/config/json-write/pi-config-json-pretty-test.scm
bin/gf test tools/pi/tests/liii/pi/code/config/json-write/pi-write-atomic-test.scm
bin/gf test tools/pi/tests/liii/pi/code/config/json-write/pi-apply-writes-test.scm
```

## 测试文件索引

- `pi-config-json-pretty-test.scm` — 2 空格缩进、保序的 JSON 序列化
- `pi-write-atomic-test.scm` — 单文件原子写入与临时文件清理
- `pi-apply-writes-test.scm` — 多文件顺序写入与父目录创建
