# (liii uri parse) 模块函数分类索引

`(liii uri parse)` 提供 URI 网络位置（netloc）的解析和构造辅助函数。

使用时导入：

```scheme
(import (liii uri parse))
```

## 常见用法示例

```scheme
(parse-netloc "user:pass@example.com:8443")
(build-netloc "user" "pass" "example.com" 8443)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
bin/gf doc "函数名"
```

或者指定库名：

```bash
bin/gf doc liii/uri/parse "函数名"
```

## 函数分类索引

### 网络位置处理
- `parse-netloc` - 将 netloc 解析为 `(user password host port)` 列表
- `build-netloc` - 从用户名、密码、主机和端口构造 netloc 字符串
