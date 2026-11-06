# (liii hashlib) 模块函数分类索引

`(liii hashlib)` 提供常用消息摘要算法：MD5、SHA-1、SHA-256，
支持对字符串直接求摘要，也支持对文件流式求摘要，
适合校验和计算、缓存键生成与文件完整性核对。

## 常见用法示例

```scheme
(import (liii hashlib))

(md5 "hello")          ; => "5d41402abc4b2a76b9719d911017c592"
(sha1 "hello")         ; => "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"
(sha256 "hello")       ; => "2cf24dba5fb0a30e26e83b2ac5b9e29e..."
(sha256-by-file "a.txt")  ; 对文件求 SHA-256
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/hashlib "函数名"
```

## 函数分类索引

### 一、字符串摘要
- `md5` - 计算字符串的 MD5 摘要
- `sha1` - 计算字符串的 SHA-1 摘要
- `sha256` - 计算字符串的 SHA-256 摘要

### 二、文件摘要
- `md5-by-file` - 计算文件的 MD5 摘要
- `sha1-by-file` - 计算文件的 SHA-1 摘要
- `sha256-by-file` - 计算文件的 SHA-256 摘要

## 错误处理约定

- 参数不是字符串（或文件不存在）时抛出底层错误
