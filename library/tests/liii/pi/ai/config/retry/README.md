# (liii pi ai config retry)

重试策略配置 `<pi-retry-config>` 记录与序列化原语。

## 测试命令

```bash
bin/gf test library/tests/liii/pi/ai/config/retry/
```

## 函数分类

- `pi-retry-config` - 构造带默认值与关键字参数的 `<pi-retry-config>` 记录
- `pi-retry-config?` - 判断对象是否为 `<pi-retry-config>` 记录
- `pi-retry-config-enabled` - 获取是否开启重试
- `pi-retry-config-max-retries` - 获取客户端级最大重试次数
- `pi-retry-config-base-delay-ms` - 获取基础退避延迟毫秒数
- `pi-retry-config-max-agent-delay-ms` - 获取最大退避延迟毫秒数
- `pi-retry-config-provider-max-retries` - 获取服务商级最大重试次数
- `pi-retry-config->json` - 将 `<pi-retry-config>` 转为 JSON 关联列表
- `json->pi-retry-config` - 从 JSON 关联列表解析 `<pi-retry-config>`
