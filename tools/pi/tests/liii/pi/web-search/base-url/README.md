# (liii pi web-search base-url) 模块函数分类索引

`(liii pi web-search base-url)` 提供联网搜索的基础 URL（baseUrl）解析能力，覆盖会话配置、provider 本地配置与环境变量等来源。

使用时导入：

```scheme
(import (liii pi web-search base-url))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-resolve-search-base-url
gf doc pi-resolve-deepseek-search-base-url
gf doc pi-web-search-config-base-url
```

## 函数分类索引

### 端点解析
- `pi-resolve-search-base-url` - 解析用于 web_search 联网搜索的基础 URL
- `pi-resolve-deepseek-search-base-url` - 解析 deepseek 联网搜索使用的基础 URL
- `pi-web-search-config-base-url` - 从 web-search 配置中读取基础 URL
