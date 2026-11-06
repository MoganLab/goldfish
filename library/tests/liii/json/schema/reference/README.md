# (liii json schema reference) 模块函数分类索引

`(liii json schema reference)` 实现 JSON Schema 的引用解析：
解析 `$ref`、`$dynamicRef` 与锚点引用，并把动态作用域纳入锚点查找。

引用解析按「引用 + 根资源 + 动态作用域」三元组缓存，
不同校验器之间互不共享缓存条目。

## 常见用法示例

```scheme
(import (liii json schema reference))

(resolve-reference validator "#/$defs/x" root)   ; => 引用指向的资源
(resolve-reference-root validator "#/$defs/x" root)   ; => 引用所属的根资源
(call-with-dynamic-scope resource (lambda () ...))   ; => 在动态作用域中执行
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/reference "函数名"
```

## 函数分类索引

### 一、引用解析
- `resolve-reference` - 解析引用，返回引用指向的资源
- `resolve-reference-root` - 解析引用并返回它所属的根资源
- `resolve-reference-with-root` - 解析引用并同时返回资源与它所属的根资源
- `resolve-dynamic-reference` - 解析动态引用，返回引用指向的资源

### 二、动态作用域
- `call-with-dynamic-scope` - 在动态作用域中调用过程
- `reset-dynamic-scope!` - 清空当前动态作用域

## 错误处理约定

- 引用不是字符串、目标不存在或 `retrieve` 返回非法资源时抛出 `json-schema-unresolvable`
- `resolve-reference*` 会把检索到的资源补上 `$id`，使资源内的相对引用有正确的基准
- 动态作用域是模块级全局状态，异常中断后应调用 `reset-dynamic-scope!` 复位
