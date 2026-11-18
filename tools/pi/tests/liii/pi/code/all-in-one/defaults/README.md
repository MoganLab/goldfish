# (liii pi code all-in-one defaults) 模块函数分类索引

`(liii pi code all-in-one defaults)` 是 all-in-one 默认配置的**唯一真值源**：默认链（默认伪模型）与默认全局调度参数只在这里构造。

provider 目录回退、request 回退、压缩窗口回退、配置 TUI 回退，以及首次生成的 `all-in-one.json`，全部从本模块派生，不再各自手写一份默认链 JSON 或重复书写 `(make-all-in-one-cfg #t 3600000 60000 8000 ...)`。

使用时导入：

```scheme
(import (liii pi code all-in-one defaults))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc all-in-one-default-config-name
gf doc all-in-one-default-chains
gf doc make-default-all-in-one-config
```

## 函数分类索引

- `all-in-one-default-config-name` - 默认配置文件的展示名称（仅在首次生成 `all-in-one.json` 时写入根节点 `name`）
- `all-in-one-default-chains` - 内置默认链定义列表
- `make-default-all-in-one-config` - 构造默认全局配置记录（含默认链）

## 默认值

| 项 | 值 |
| --- | --- |
| `continuous` | `#t` |
| `maxWaitMs` | `3600000`（1 小时） |
| `retryIntervalMs` | `60000`（60 秒） |
| `probeTimeoutMs` | `8000`（8 秒） |

默认两条链：

| 链 ID | 展示名 | 节点（按优先级） |
| --- | --- | --- |
| `coding-fast` | 快速 | `google/gemini-3.8-flash`(medium) → `minimax-cn/MiniMax-M3.1-Flash`(medium) → `deepseek/deepseek-v4-pro`(high) |
| `coding-high-quality` | 高质量 | `kimi-coding/k3.1-pro`(high) → `zai-coding-cn/glm-5.3`(high) |

## 行为契约

- 链 ID、展示名、节点顺序与每个节点的三元组属于兼容性契约，测试逐项断言；
- 每次调用返回新记录，调用方改写字段不会污染其他调用；
- 默认配置文件由 `make-default-all-in-one-config` 序列化生成，不维护第二份手写默认链 JSON；
- 已存在的用户配置文件绝不因默认值重构而被覆盖。
