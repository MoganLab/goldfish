# (liii pi code all-in-one types) 模块函数分类索引

`(liii pi code all-in-one types)` 定义 all-in-one 元供应商的核心记录类型，包括链节点 `<chain-node>`、链定义 `<chain-def>` 与全局配置 `<all-in-one-cfg>` 及其构造函数与访问器。

使用时导入：

```scheme
(import (liii pi code all-in-one types))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-chain-node
gf doc chain-node?
gf doc make-chain-def
gf doc chain-def?
gf doc make-all-in-one-cfg
gf doc all-in-one-cfg?
```

## 函数分类索引

### 链节点记录 (<chain-node>)

- `make-chain-node` - 构造三元组链节点记录（provider, model, thinkingLevel）
- `chain-node?` - 判断是否为链节点记录
- `chain-node-provider` - 获取链节点的供应商 ID
- `chain-node-model` - 获取链节点的模型 ID
- `chain-node-thinking-level` - 获取链节点的思考等级

### 链定义记录 (<chain-def>)

- `make-chain-def` - 构造链定义记录（id, name, nodes）
- `chain-def?` - 判断是否为链定义记录
- `chain-def-id` - 获取链定义的唯一标识
- `chain-def-name` - 获取链定义的展示名称
- `chain-def-nodes` - 获取链定义的节点列表

### 全局配置记录 (<all-in-one-cfg>)

- `make-all-in-one-cfg` - 构造 all-in-one 全局配置记录
- `all-in-one-cfg?` - 判断是否为 all-in-one 全局配置记录
- `all-in-one-cfg-continuous?` - 获取是否开启连续等候
- `all-in-one-cfg-max-wait-ms` - 获取全链等待单次最大耗时（毫秒）
- `all-in-one-cfg-retry-interval-ms` - 获取全链等待重试轮询间隔（毫秒）
- `all-in-one-cfg-probe-timeout-ms` - 获取单节点预检探测超时（毫秒）
- `all-in-one-cfg-chains` - 获取链定义列表
