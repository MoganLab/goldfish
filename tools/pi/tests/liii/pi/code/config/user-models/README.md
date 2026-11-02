# (liii pi code config user-models) 测试导览

`(liii pi code config user-models)` 合成内置目录、provider 文件与用户模型文件，
并判断模型是否存在。

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/config/user-models/
```

## 测试文件索引

- `pi-user-model-exists-p-test.scm` — 模型存在性判定
- `pi-user-provider-models-test.scm` — provider 可用模型列表
- `pi-user-provider-model-list-test.scm` — 配置视图完整模型列表与 enabled 标记
