# (liii pi ai config settings)

客户端全局偏好配置 `<pi-settings>` 记录与序列化原语。

## 测试命令

```bash
bin/gf test library/tests/liii/pi/ai/config/settings/
```

## 函数分类

- `pi-settings` - 构造带默认值与关键字参数的 `<pi-settings>` 记录
- `pi-settings?` - 判断对象是否为 `<pi-settings>` 记录
- `pi-settings-default-provider` - 获取默认服务商名称
- `pi-settings-default-model` - 获取默认模型标识符
- `pi-settings-default-thinking-level` - 获取默认思考级别
- `pi-settings-http-proxy` - 获取 HTTP 代理地址
- `pi-settings-retry` - 获取重试策略配置
- `pi-settings-default-tools` - 获取默认启用的工具列表
- `pi-settings-session-dir` - 获取会话数据保存目录
- `pi-settings-hide-thinking-block` - 获取是否隐藏思考内容块
- `pi-settings-model-thinking-levels` - 获取按模型覆盖的思考级别关联表
- `pi-settings-enable-skill-commands` - 获取 Agent Skills 命令开关
- `pi-settings-model-thinking-level` - 查询指定服务商与模型的思考级别
- `pi-settings->alist` - 将 `<pi-settings>` 转为关联列表
- `pi-settings->json` - 将 `<pi-settings>` 转为 JSON 关联列表
- `json->pi-settings` - 从 JSON 关联列表解析 `<pi-settings>`
