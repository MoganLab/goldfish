# (liii pi code all-in-one provider) 模块函数分类索引

`(liii pi code all-in-one provider)` 提供元供应商目录条目装配，以及将配置中的每一条链式（chain）以伪模型（pseudo-model）形式投射为标准模型元数据的能力。

使用时导入：

```scheme
(import (liii pi code all-in-one provider))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc all-in-one-chain->model
gf doc all-in-one-chains->models
gf doc all-in-one-models
gf doc all-in-one-provider-available?
gf doc all-in-one-provider-entry
```

## 函数分类索引

- `all-in-one-chain->model` - 将单条链式记录转换为标准模型元数据 alist
- `all-in-one-chains->models` - 将链式记录列表批量转换为模型元数据 alist 列表
- `all-in-one-models` - 从指定配置目录加载并发现伪模型列表
- `all-in-one-provider-available?` - 检查元供应商在给定配置目录下是否有效可用
- `all-in-one-provider-entry` - 构造元供应商的完整目录条目
