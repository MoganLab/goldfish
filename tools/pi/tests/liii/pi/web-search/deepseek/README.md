# (liii pi web-search deepseek) 模块函数分类索引

`(liii pi web-search deepseek)` 提供 deepseek 联网搜索接口的请求构建、响应解析与文本格式化能力。

使用时导入：

```scheme
(import (liii pi web-search deepseek))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc deepseek-build-search-request-body
gf doc deepseek-build-fetch-request-body
gf doc deepseek-build-search-endpoint
gf doc deepseek-build-fetch-endpoint
gf doc deepseek-parse-search-results
gf doc deepseek-sources-from-results
gf doc deepseek-format-search-text
gf doc deepseek-parse-fetch-page
gf doc deepseek-append-fetched-page
```

## 函数分类索引

### 请求构建
- `deepseek-build-search-request-body` - 构建 deepseek 搜索接口的请求体
- `deepseek-build-fetch-request-body` - 构建 deepseek 页面抓取接口的请求体
- `deepseek-build-search-endpoint` - 构建 deepseek 搜索接口的完整 URL
- `deepseek-build-fetch-endpoint` - 构建 deepseek 页面抓取接口的完整 URL

### 响应解析
- `deepseek-parse-search-results` - 解析 deepseek 搜索接口返回的 JSON 结果数据
- `deepseek-parse-fetch-page` - 解析 deepseek 抓取接口返回的页面数据

### 结果展示
- `deepseek-sources-from-results` - 从 deepseek 搜索结果中提取来源列表
- `deepseek-format-search-text` - 将 deepseek 搜索结果列表格式化为可读文本
- `deepseek-append-fetched-page` - 将抓取到的页面内容追加到搜索结果正文末尾
