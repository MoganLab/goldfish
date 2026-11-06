# (liii uri compare) 模块函数分类索引

`(liii uri compare)` 提供 URI 的相等比较、字典序比较和哈希计算能力。

使用时导入：

```scheme
(import (liii uri compare))
```

## 常见用法示例

```scheme
(uri=?
  (make-uri-raw "https" "example.com" "/" '() #f)
  (make-uri-raw "https" "example.com" "/" '() #f))

(uri-hash (make-uri-raw "https" "example.com" "/path" '() #f))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
bin/gf doc "函数名"
```

或者指定库名：

```bash
bin/gf doc liii/uri/compare "函数名"
```

## 函数分类索引

### 比较函数
- `uri=?` - 检查两个 URI 是否相等
- `uri<?` - 检查第一个 URI 是否小于第二个
- `uri>?` - 检查第一个 URI 是否大于第二个

### 哈希函数
- `uri-hash` - 计算 URI 的哈希值
