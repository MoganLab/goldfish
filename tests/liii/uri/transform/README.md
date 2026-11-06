# (liii uri transform) 模块函数分类索引

`(liii uri transform)` 提供 URI 组件替换、查询参数更新、路径操作与 URI 归一化能力。

使用时导入：

```scheme
(import (liii uri transform))
```

## 常见用法示例

```scheme
(uri-with-scheme (make-uri-raw "http" "example.com" "/" '() #f) "https")
(uri-extend-query (make-uri-raw "https" "api.com" "/" '() #f)
  '(("page" . "1") ("limit" . "10")))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
bin/gf doc "函数名"
```

或者指定库名：

```bash
bin/gf doc liii/uri/transform "函数名"
```

## 函数分类索引

### 替换 URI 组件
- `uri-with-scheme` - 替换 scheme
- `uri-with-host` - 替换 host
- `uri-with-port` - 替换或移除 port
- `uri-with-path` - 替换 path
- `uri-with-fragment` - 替换 fragment

### 查询参数更新
- `uri-update-query` - 使用函数更新 query
- `uri-extend-query` - 扩展 query
- `uri-without-query` - 移除全部 query
- `uri-without-query-param` - 移除指定 query 参数

### 路径操作与归一化
- `uri-join-path` - 在 path 后追加路径段
- `uri-with-name` - 替换路径末级名称
- `uri-with-suffix` - 替换文件后缀
- `uri-join` - 合并两个 URI
- `uri-normalize` - 按协议归一化 URI
