# (liii pi web-search proxy) 模块函数分类索引

`(liii pi web-search proxy)` 提供联网搜索的代理地址解析能力，覆盖会话配置、provider 本地配置与环境变量等来源。

使用时导入：

```scheme
(import (liii pi web-search proxy))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-resolve-search-proxy-url
gf doc pi-resolve-search-proxy-alist
gf doc pi-web-search-config-proxy
```

## 函数分类索引

### 代理解析
- `pi-resolve-search-proxy-url` - 解析用于 web_search 联网搜索的 HTTP/HTTPS 代理 URL
- `pi-resolve-search-proxy-alist` - 将代理地址转换为 HTTP 客户端所需的代理 alist
- `pi-web-search-config-proxy` - 从 web-search 配置中读取代理地址
