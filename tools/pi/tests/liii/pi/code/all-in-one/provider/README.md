# (liii pi code all-in-one provider) 模块函数分类索引

`(liii pi code all-in-one provider)` 负责把配置中的每一条链以**伪模型**形式投影成标准模型目录条目，并回答元供应商是否对用户可见。它不维护默认链：默认链与默认配置的唯一真值源在 `(liii pi code all-in-one defaults)`，加载与缺省回退在 `(liii pi code all-in-one loader)`。

使用时导入：

```scheme
(import (liii pi code all-in-one provider))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc all-in-one-models
gf doc all-in-one-provider-entry
gf doc all-in-one-provider-available?
gf doc chain-conservative-context-window
```

## 函数分类索引

### 模型目录

- `all-in-one-models` - 从指定配置目录加载配置（无有效配置时回退默认配置）并返回伪模型元数据列表
- `all-in-one-provider-entry` - 构造元供应商的完整目录条目

### 可用性

- `all-in-one-provider-available?` - 是否存在可解析的用户配置且至少配置了一条链

### 链元数据

- `chain-conservative-context-window` - 链内所有节点上下文窗口的最小值（保守兜底）

## 行为契约

- `all-in-one-models` 始终返回非空列表：未配置或配置非法时回退 `defaults` 的默认链，保证伪模型始终可选；
- `all-in-one-provider-available?` 只表示“用户显式配置过 all-in-one”，不因默认回退而恒为真，因此不会被误当成内置 provider 反复出现在目录里；判定本身不读默认链；
- 伪模型元数据固定为 `provider=all-in-one`、`api=all-in-one`、`reasoning=#t`；
- 单链投影 `%chain->model` 是实现细节，保持私有；
- `all-in-one-provider-entry` 保留为模型目录条目的构造接口，当前只有本模块与目录测试使用；
- 上下文窗口取链内最小值，避免压缩判定按最强节点高估容量。
