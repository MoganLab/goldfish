# (liii pi code config user-providers) 测试导览

`(liii pi code config user-providers)` 读取 provider 白名单与 provider 层配置文件。

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/config/user-providers/
```

## 测试文件索引

- `pi-providers-whitelist-path-test.scm` — 白名单文件路径优先级
- `pi-load-providers-whitelist-test.scm` — 白名单读取与错误处理
- `pi-user-provider-file-test.scm` — provider 文件读取
- `pi-user-normalize-api-test.scm` — api 简写归一化
- `pi-alist-set-key-test.scm` — alist 键替换与追加
- `pi-providers-json-test.scm` — 白名单 JSON 序列化
- `pi-providers-write-writes-test.scm` — 白名单写入计划
- `pi-provider-file-with-models-test.scm` — provider 文件 models 白名单重写
