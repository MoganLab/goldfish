# (hint liii json) 模块导览

`(hint liii json)` 是 `(liii json)` 的类型接口库，只保存类型签名数据，
不导出 `(liii json)` 的任何过程，也不会影响 `(liii json)` 的运行行为。

使用时导入：

```scheme
(import (hint liii json))
```

查询某个过程的签名：

```scheme
(hint-liii/json 'json-ref)
;; => ((any? any? -> any?) (any? any? any? -> any?))
```

未登记签名的过程返回 `#f`。

## 类型约定

| 约定 | 说明 |
|---|---|
| JSON 值 | 库未提供 `json?` 谓词，统一用 `any?` 表示 |
| 键 | 可以是 `symbol?` / `string?` / `integer?` / `boolean?`；`json-set` / `json-push` / `json-reduce` 还接受 `#t` / `#f` / 过程作为匹配键 |
| 安全获取器 | 默认值常为 `#f`，参数与返回值都用 `(or #f T)` 表示 |
| 可变参数 | 按常见调用形态登记若干条签名，关键字参数以 `any?` 占位 |

## 回归测试

| 文件 | 说明 |
|---|---|
| `json-test.scm` | 校验 `(liii json)` 的全部导出过程均已登记签名 |
