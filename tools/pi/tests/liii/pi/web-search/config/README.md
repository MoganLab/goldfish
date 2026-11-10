# (liii pi web-search config) 模块函数分类索引

`(liii pi web-search config)` 提供联网搜索配置的读取与解析能力，覆盖 provider、API Key、基础 URL、模型标识与代理地址。

使用时导入：

```scheme
(import (liii pi web-search config))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-load-web-search-config
gf doc pi-resolve-web-search-provider
gf doc pi-resolve-google-api-key
gf doc pi-resolve-deepseek-api-key
gf doc pi-resolve-search-model-id
gf doc pi-web-search-config-model
```

## 函数分类索引

### 配置文件读取
- `pi-load-web-search-config` - 从配置目录读取 web-search.json 配置文件

### provider 解析
- `pi-resolve-web-search-provider` - 解析联网搜索使用的后端 provider

### 凭据与地址解析
- `pi-resolve-google-api-key` - 解析 Google 联网搜索使用的 API Key
- `pi-resolve-deepseek-api-key` - 解析 deepseek 联网搜索使用的 API Key
- `pi-resolve-search-model-id` - 解析联网搜索使用的模型标识
- `pi-web-search-config-model` - 从 web-search 配置中解析联网搜索使用的模型标识
