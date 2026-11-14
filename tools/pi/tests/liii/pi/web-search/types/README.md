# (liii pi web-search types)

`web-search` 配置记录类型与 JSON 转换函数。

## 测试命令

```bash
bin/gf test tools/pi/tests/liii/pi/web-search/types/
```

## 函数列表

- `make-pi-web-search-config` - 构造 `web-search` 配置记录
- `pi-web-search-config?` - 判断是否为 `pi-web-search-config` 记录
- `pi-web-search-config-provider` - 获取搜索后端名称
- `pi-web-search-config-raw-model` - 获取原始模型标识
- `pi-web-search-config-raw-base-url` - 获取原始基础 URL
- `pi-web-search-config-raw-proxy` - 获取原始代理 URL
- `json->pi-web-search-config` - 将 JSON 对象转换为 `pi-web-search-config` 记录
