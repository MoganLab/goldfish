# (liii pi code config) 测试导览

`(liii pi code config)` 负责 `gf pi` 的运行配置解析：命令行参数、环境变量、
`settings.json` / `auth.json`、provider 与模型目录合并、代理与凭据解析，
以及 config TUI 使用的配置写入辅助函数。

## 模块组成

- `tools/pi/liii/pi/code/config.scm` — 配置聚合入口与运行配置解析
- `tools/pi/liii/pi/code/config/settings.scm` — `settings.json` 读写
- `tools/pi/liii/pi/code/config/user-catalog.scm` — 用户 provider/模型目录
- `tools/pi/liii/pi/code/config/user-providers.scm` — provider 白名单与自定义 provider
- `tools/pi/liii/pi/code/config/user-models.scm` — 用户模型文件读取
- `tools/pi/liii/pi/code/config/json-write.scm` — JSON 配置写入

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/config/resolve-pi-config-test.scm
bin/gf test tools/pi/tests/liii/pi/code/config/resolve-pi-config-user-catalog-test.scm
bin/gf test tools/pi/tests/liii/pi/code/config/resolve-pi-config-dir-test.scm
bin/gf test tools/pi/tests/liii/pi/code/config/resolve-pi-config-env-test.scm
```

## 测试文件索引

### 运行配置解析

- `resolve-pi-config-test.scm` — 默认解析、CLI/环境变量覆盖、未知模型报错
- `resolve-goldpi-config-test.scm` — `resolve-goldpi-config` 兼容别名
- `resolve-pi-config-user-catalog-test.scm` — 用户 provider/模型白名单与目录合并
- `resolve-pi-config-dir-test.scm` — config-dir 与 agent-dir 分离
- `resolve-pi-config-env-test.scm` — 配置解析的环境隔离
- `resolve-active-tools-test.scm` — 工具列表解析与默认集合并
- `config-worker-registry-test.scm` — worker 会话中的扩展注册表与默认工具
- `augment-pi-config-test.scm` — 扩展 config augmenter 链式增强
- `pi-missing-credential-msg-test.scm` — 凭证缺失提示

### 设置读写

- `pi-load-settings-test.scm` — `settings.json` 加载与字段回退
- `pi-read-json-file-test.scm` — JSON 文件读取
- `json-dict-ref-test.scm` — JSON 字典按键读取
- `pi-load-compaction-settings-test.scm` — compaction 设置读取
- `pi-save-default-model-bang-test.scm` — 保存默认 provider/模型
- `pi-save-default-thinking-level-bang-test.scm` — 保存默认思考级别
- `pi-save-hide-thinking-block-bang-test.scm` — 保存思考块显示开关
- `gf-get-config-dir-test.scm` — JSON 配置文件目录解析
- `pi-get-agent-dir-test.scm` — agent 基准目录解析

### 目录与凭据

- `user-catalog-test.scm` — 用户 provider/模型目录读取
- `pi-load-auth-test.scm` — `auth.json` 凭据加载
- `pi-resolve-provider-key-test.scm` — provider API 密钥解析
- `pi-resolve-config-value-test.scm` — 配置值环境变量插值
- `pi-resolve-model-metadata-test.scm` — 模型元数据解析
- `pi-resolve-proxy-url-test.scm` — 代理 URL 解析
- `pi-cfg-provider-proxy-test.scm` — provider 级代理重算
- `pi-cfg-provider-api-test.scm` — provider 协议标识解析
- `pi-cfg-config-dir-test.scm` — 从运行配置取配置文件目录

### 模型能力

- `pi-model-supports-thinking-p-test.scm` — 模型 reasoning 能力判定
- `pi-model-available-thinking-levels-test.scm` — 模型可用思考级别
