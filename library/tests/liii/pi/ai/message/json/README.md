# (liii pi ai message json) 模块函数索引

`(liii pi ai message json)` 提供将 `(liii json)` 解析出的 Scheme-JSON 对象反序列化为消息与内容块 record 的接口。

使用时导入：

```scheme
(import (liii pi ai message json))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc json->pi-message
```

## 函数分类索引

### JSON 反序列化
- `json->pi-block` - Scheme-JSON 对象转内容块 record
- `json->pi-message` - Scheme-JSON 对象转消息 record
