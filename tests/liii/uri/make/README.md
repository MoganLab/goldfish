# (liii uri make) 模块函数分类索引

`(liii uri make)` 提供从 URI 字符串或独立组件构造 URI 记录的能力。

使用时导入：

```scheme
(import (liii uri make))
```

## 常见用法示例

```scheme
(make-uri "https://example.com/path?a=1#frag")

(uri-build
  :scheme "https"
  :host "example.com"
  :path "/api/v1"
  :query '(("key" . "value")))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
bin/gf doc "函数名"
```

或者指定库名：

```bash
bin/gf doc liii/uri/make "函数名"
```

## 函数分类索引

### 从字符串构造
- `make-uri` - 从字符串解析构造 URI
- `string->uri` - `make-uri` 的别名

### 从组件构建
- `uri-build` - 使用关键字参数从组件构建 URI
