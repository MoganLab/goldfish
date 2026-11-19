# (liii pi code all-in-one config) 模块函数分类索引

`(liii pi code all-in-one config)` 定义 all-in-one 全局配置记录 `<all-in-one-cfg>`：连续等候开关、全链最长等待、失败冷却/重试间隔、单节点探活超时与链定义列表。record 定义、构造器、访问器、秒/毫秒时长解析与 JSON 双向转换都放在同一文件中。

`types` 只重导出这些名字，不保存实现；生产代码应继续通过 `(liii pi code all-in-one types)` 使用领域接口。

使用时导入：

```scheme
(import (liii pi code all-in-one config))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-all-in-one-cfg
gf doc all-in-one-cfg?
gf doc all-in-one-cfg-continuous?
gf doc all-in-one-cfg-max-wait-ms
gf doc all-in-one-cfg-retry-interval-ms
gf doc all-in-one-cfg-probe-timeout-ms
gf doc all-in-one-cfg-chains
gf doc all-in-one-cfg->json
gf doc json->all-in-one-cfg
```

## 函数分类索引

### 构造

- `make-all-in-one-cfg` - 构造全局配置记录；默认 `continuous=#t`、`maxWaitMs=3600000`、`retryIntervalMs=60000`、`probeTimeoutMs=8000`、`chains='()`

### 谓词

- `all-in-one-cfg?` - 判断是否为全局配置记录

### 属性访问

- `all-in-one-cfg-continuous?` - 是否启用全链枯竭后的连续等候
- `all-in-one-cfg-max-wait-ms` - 单次连续故障 episode 的累计等待预算（毫秒）
- `all-in-one-cfg-retry-interval-ms` - 节点失败冷却窗口与全链等待的重试轮询间隔（毫秒）
- `all-in-one-cfg-probe-timeout-ms` - 单节点探活超时（毫秒）
- `all-in-one-cfg-chains` - 链定义列表

### JSON 转换

- `all-in-one-cfg->json` - 序列化为 JSON 对象，统一输出毫秒字段（`maxWaitMs` / `retryIntervalMs` / `probeTimeoutMs`），入参非 record 时抛 `type-error`
- `json->all-in-one-cfg` - 解析 JSON 对象；已是 record 时幂等返回；根不是对象或 `chains` 不是数组时返回 `#f`

## 配置解析优先级

内部时长解析顺序即协议不变量：秒级别名 → 毫秒键 → 默认值，秒级值乘 1000 转为内部毫秒。

| 内部字段 | 秒级别名（优先级从高到低） | 毫秒键 | 默认值 |
| --- | --- | --- | --- |
| `maxWaitMs` | `maxWait`、`maxWaitSeconds` | `maxWaitMs` | `3600000` |
| `retryIntervalMs` | `retryInterval`、`cooldown`、`retryIntervalSeconds`、`cooldownSeconds` | `retryIntervalMs` | `60000` |
| `probeTimeoutMs` | `probeTimeout`、`probeTimeoutSeconds` | `probeTimeoutMs` | `8000` |

只接受大于 0 的数值，0、负数与非数值一律视为未配置并继续回退。

## 行为契约

- 非法配置不抛异常：非对象、`chains` 非数组都返回 `#f`，由 `loader` 的缺省回退入口决定是否使用默认配置；
- 已有的 JSON 字段名与秒/毫秒别名保持不变，序列化统一输出毫秒字段，两种表示在语义上等价；
- 不提供 setter，也不提供 `xxx->alist` 之类的第二形态转换。
