# (liii pi code config) 配置解析与用户目录测试导览

`(liii pi code config)` 负责解析 `gf pi` 的命令行参数与用户配置目录：
`settings.json`、`auth.json`、`models/providers.json` 白名单以及
`models/<provider>/<model>.json` 模型目录。测试覆盖配置加载、provider
与模型解析、API key/代理选择、思考级别以及配置写回。

## 模块组成

- `tools/pi/liii/pi/code/config.scm` — 配置解析入口与公开 API
- `tools/pi/liii/pi/code/config/user-providers.scm` — provider 白名单加载
- `tools/pi/liii/pi/code/config/user-catalog.scm` — 用户模型目录构建
- `tools/pi/liii/pi/code/config/user-models.scm` — 模型元数据加载
- `tools/pi/liii/pi/code/config/user-fs.scm` — 用户配置文件读写
- `tools/pi/liii/pi/code/config/settings.scm` — settings.json 解析

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/config/
```

## 测试文件索引

- `resolve-pi-config-test.scm` — `resolve-pi-config`：默认配置、命令行覆盖
- `resolve-pi-config-env-test.scm` — 环境变量污染隔离
- `resolve-pi-config-dir-test.scm` — 配置目录优先级
- `resolve-pi-config-user-catalog-test.scm` — 用户目录驱动的 provider/模型路由
- `resolve-goldpi-config-test.scm` — `resolve-goldpi-config` 别名
- `resolve-active-tools-test.scm` — 生效工具列表计算
- `config-worker-registry-test.scm` — worker 配置注册
- `gf-get-config-dir-test.scm` — `gf-get-config-dir` 目录选择
- `pi-cfg-config-dir-test.scm` — 从 cfg 读取配置目录
- `pi-cfg-provider-api-test.scm` — provider 协议标识选择
- `pi-cfg-provider-proxy-test.scm` — provider 级代理选择
- `pi-resolve-proxy-url-test.scm` — 全局代理 URL 解析
- `pi-model-available-thinking-levels-test.scm` — 模型可用思考级别
- `pi-model-supports-thinking-p-test.scm` — 模型是否支持思考级别
- `pi-missing-credential-msg-test.scm` — 凭证缺失提示
- `augment-pi-config-test.scm` — 扩展 augmenter 增强配置
