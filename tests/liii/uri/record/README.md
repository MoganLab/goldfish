# (liii uri record) 模块函数分类索引

`(liii uri record)` 定义 URI 记录类型，并提供原始字段访问器、组件访问器、路径与查询参数处理、编码和解码能力。

使用时导入：

```scheme
(import (liii uri record))
```

## 常见用法示例

```scheme
(define u
  (make-uri-raw "https" "user@example.com:8080" "/path"
    '(("a" . "1")) "frag"))

(uri-host u)
(uri-query-ref u "a")
(uri-fragment u)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
bin/gf doc "函数名"
```

或者指定库名：

```bash
bin/gf doc liii/uri/record "函数名"
```

## 函数分类索引

### 构造函数与谓词
- `make-uri-raw` - 从组件创建原始 URI 记录
- `uri?` - 检查对象是否为 URI 记录
- `uri-error` - 抛出 URI 错误

### 原始字段访问器与修改器
- `uri-scheme-raw`、`uri-scheme-set!` - 读取、修改 scheme 字段
- `uri-netloc-raw`、`uri-netloc-set!` - 读取、修改 netloc 字段
- `uri-path-raw`、`uri-path-set!` - 读取、修改 path 字段
- `uri-query-raw`、`uri-query-set!` - 读取、修改 query 字段
- `uri-fragment-raw`、`uri-fragment-set!` - 读取、修改 fragment 字段

### URI 组件访问器
- `uri-scheme`、`uri-raw-scheme` - 获取 scheme
- `uri-host`、`uri-raw-host` - 获取 host
- `uri-port`、`uri-explicit-port` - 获取实际端口或显式端口
- `uri-user`、`uri-raw-user` - 获取用户名
- `uri-password`、`uri-raw-password` - 获取密码
- `uri-authority`、`uri-raw-authority` - 获取完整 authority
- `uri-path`、`uri-raw-path` - 获取 path
- `uri-query`、`uri-query-string` - 获取查询参数或查询字符串
- `uri-query-ref`、`uri-query-ref*` - 按名称读取查询参数
- `uri-fragment`、`uri-raw-fragment` - 获取 fragment

### 路径处理
- `uri-path->list` - 将 path 分割为路径段列表
- `uri-parent` - 获取父目录路径
- `uri-name` - 获取路径末级名称
- `uri-suffix`、`uri-suffixes` - 获取文件后缀

### 编码与解码
- `uri-encode`、`uri-decode` - 百分比编码与解码
- `uri-encode-path`、`uri-decode-path` - 路径编码与解码

### 查询字符串处理
- `query-string->alist` - 将查询字符串解析为 alist
- `alist->query-string` - 将 alist 转换为查询字符串

### 网络位置辅助函数
- `parse-netloc` - 解析 netloc 字符串（由 `(liii uri parse)` 定义）
- `build-netloc` - 从组件构建 netloc 字符串（由 `(liii uri parse)` 定义）
