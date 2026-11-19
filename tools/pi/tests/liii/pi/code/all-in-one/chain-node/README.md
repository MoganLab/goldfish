# (liii pi code all-in-one chain-node) 模块函数分类索引

`(liii pi code all-in-one chain-node)` 定义 all-in-one 链内的物理候选节点记录 `<chain-node>`：一个节点由真实供应商、真实模型与该节点的默认思考等级三元组构成。record 定义、构造器、访问器与 JSON 双向转换都放在同一文件中。

`types` 只重导出这些名字，不保存实现；生产代码应继续通过 `(liii pi code all-in-one types)` 使用领域接口。

使用时导入：

```scheme
(import (liii pi code all-in-one chain-node))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-chain-node
gf doc chain-node?
gf doc chain-node-provider
gf doc chain-node-model
gf doc chain-node-thinking-level
gf doc chain-node->json
gf doc json->chain-node
```

`%non-empty-string?` 是 chain-node 与 chain-def 共享的底层原语（带 `%` 前缀，仅供同库兄弟模块导入），因此不提供独立单测文件，由两个模块的 `json->*` 解析测试覆盖。

## 函数分类索引

### 构造

- `make-chain-node` - 构造三元组链节点记录（provider, model, thinkingLevel），`thinkingLevel` 缺省为 `off`

### 谓词

- `chain-node?` - 判断是否为链节点记录

### 属性访问

- `chain-node-provider` - 获取节点的真实供应商标识
- `chain-node-model` - 获取节点的真实模型标识
- `chain-node-thinking-level` - 获取节点的默认思考等级

### JSON 转换

- `chain-node->json` - 将记录序列化为 JSON 对象（`provider` / `model` / `thinkingLevel`），入参非 record 时抛 `type-error`
- `json->chain-node` - 从 JSON 对象解析节点；已是 record 时幂等返回；缺 `provider` 或 `model` 时告警并返回 `#f` 以便调用方跳过该节点

## 行为契约

- JSON 字段名固定为 `provider`、`model`、`thinkingLevel`，不接受其他键名；
- 非法或空白的 `thinkingLevel` 回退为 `off`；
- 解析失败不抛异常，而是写 `pi-user-warn` 并返回 `#f`，让链定义可以跳过坏节点继续加载；
- 不提供 setter，也不提供 `xxx->alist` 之类的第二形态转换。
