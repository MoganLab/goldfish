# (liii pi code all-in-one chain-def) 模块函数分类索引

`(liii pi code all-in-one chain-def)` 定义链式定义记录 `<chain-def>`：一条链由链 ID（同时就是用户可见的伪模型 ID）、展示名称与有序候选节点列表构成。record 定义、构造器、访问器与 JSON 双向转换都放在同一文件中。

`types` 只重导出这些名字，不保存实现；生产代码应继续通过 `(liii pi code all-in-one types)` 使用领域接口。

使用时导入：

```scheme
(import (liii pi code all-in-one chain-def))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-chain-def
gf doc chain-def?
gf doc chain-def-id
gf doc chain-def-name
gf doc chain-def-nodes
gf doc chain-def->json
gf doc json->chain-def
```

## 函数分类索引

### 构造

- `make-chain-def` - 构造链定义记录（id, name, nodes），`nodes` 缺省为空列表

### 谓词

- `chain-def?` - 判断是否为链定义记录

### 属性访问

- `chain-def-id` - 获取链 ID（即伪模型 ID）
- `chain-def-name` - 获取链的展示名称
- `chain-def-nodes` - 获取按优先级排列的节点列表

### JSON 转换

- `chain-def->json` - 将记录序列化为 JSON 对象（`id` / `name` / `nodes` 数组），入参非 record 时抛 `type-error`
- `json->chain-def` - 从 JSON 对象解析链定义；已是 record 时幂等返回

## 行为契约

- 节点顺序即优先级顺序，序列化与解析都保序；
- 缺 `id` 时告警并返回 `#f`（链不可用）；缺 `name` 时回退为 `id`；
- `nodes` 不是数组时告警并把节点列表置空，而不是让整条链加载失败；
- 单个坏节点被跳过，合法节点保留，链顺序不变。
