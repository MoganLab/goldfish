# (liii uri predicate) 模块函数分类索引

`(liii uri predicate)` 提供 URI 的绝对性、相对性、默认端口与网络协议谓词。

使用时导入：

```scheme
(import (liii uri predicate))
```

## 常见用法示例

```scheme
(uri-absolute? (make-uri-raw "https" "example.com" "/" '() #f))
(uri-default-port "https")
(uri-network-scheme? "https")
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
bin/gf doc "函数名"
```

或者指定库名：

```bash
bin/gf doc liii/uri/predicate "函数名"
```

## 函数分类索引

### URI 类型谓词
- `uri-absolute?` - 检查是否为绝对 URI（有 scheme）
- `uri-relative?` - 检查是否为相对 URI（无 scheme）

### 端口相关
- `uri-default-port` - 获取 scheme 的默认端口
- `uri-default-port?` - 检查 URI 是否使用默认端口

### Scheme 检查
- `uri-network-scheme?` - 检查是否为网络 scheme
