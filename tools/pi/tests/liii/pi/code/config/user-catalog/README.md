# (liii pi code config user-catalog) 测试导览

`(liii pi code config user-catalog)` 装配用户 provider/模型目录，并处理默认模型的
自动激活。

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/config/user-catalog/
```

## 测试文件索引

- `pi-build-user-catalog-test.scm` — 白名单驱动的用户目录装配、白名单与路径读取
- `pi-build-config-catalog-test.scm` — 配置浏览视图（启用/禁用 provider）
- `pi-load-providers-whitelist-test.scm` — 通过 user-catalog 入口读取白名单
- `pi-user-provider-config-test.scm` — 从目录取 provider 层配置
- `pi-resolve-default-target-test.scm` — 默认 provider/model 解析与斜杠语法
- `pi-auto-activate-default-model-bang-test.scm` — 默认模型自动激活写盘
- `pi-user-model-exists-p-test.scm` — 模型存在性判定入口
