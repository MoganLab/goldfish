# (liii uri convert) 模块函数分类索引

`(liii uri convert)` 提供将 URI 记录转换为完整字符串或隐藏敏感信息的人类可读字符串的能力。

使用时导入：

```scheme
(import (liii uri convert))
```

## 常见用法示例

```scheme
(define u
  (make-uri-raw "https" "user:pass@example.com" "/path"
    '(("a" . "1")) "frag"))

(uri->string u)
(uri->human-string u)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
bin/gf doc "函数名"
```

或者指定库名：

```bash
bin/gf doc liii/uri/convert "函数名"
```

## 函数分类索引

### 字符串转换
- `uri->string` - 将 URI 转换为完整字符串
- `uri->human-string` - 生成隐藏认证信息、查询参数与片段的人类可读字符串
