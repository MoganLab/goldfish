# (liii pi ai models route) 测试导览

`(liii pi ai models route)` 负责 provider 与模型的默认值、可用性判定和路由解析。

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/models/route/pi-provider-default-model-test.scm
bin/gf test tools/pi/tests/liii/pi/ai/models/route/pi-provider-has-model-p-test.scm
bin/gf test tools/pi/tests/liii/pi/ai/models/route/pi-find-model-provider-test.scm
bin/gf test tools/pi/tests/liii/pi/ai/models/route/pi-route-provider-and-model-test.scm
bin/gf test tools/pi/tests/liii/pi/ai/models/route/pi-ensure-catalog-meta-test.scm
```

## 测试文件索引

- `pi-provider-default-model-test.scm` — `pi-provider-default-model`：各 provider 的默认模型
- `pi-provider-has-model-p-test.scm` — `pi-provider-has-model?`：模型目录可用性判定
- `pi-find-model-provider-test.scm` — `pi-find-model-provider`：按模型标识反查 provider
- `pi-route-provider-and-model-test.scm` — `pi-route-provider-and-model`：provider/模型路由与默认值回退
- `pi-ensure-catalog-meta-test.scm` — `pi-ensure-catalog-meta`：模型目录元数据补全
