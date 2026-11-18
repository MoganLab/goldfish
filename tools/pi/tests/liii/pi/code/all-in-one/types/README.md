# (liii pi code all-in-one types) 模块函数分类索引

`(liii pi code all-in-one types)` 是 all-in-one 元供应商领域记录的**聚合入口**，只按「构造 / 谓词 / 属性访问 / JSON 转换」分类重导出三个记录模块的正式接口，本身不保存任何实现。

实现分别位于平铺的：

- `(liii pi code all-in-one chain-node)` - `<chain-node>` 及 JSON 双向转换
- `(liii pi code all-in-one chain-def)` - `<chain-def>` 及 JSON 双向转换
- `(liii pi code all-in-one config)` - `<all-in-one-cfg>`、时长解析及 JSON 双向转换

生产代码统一通过本库使用领域接口；record 的单测放在各实现模块对应的测试目录中。

使用时导入：

```scheme
(import (liii pi code all-in-one types))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-chain-node
gf doc make-chain-def
gf doc make-all-in-one-cfg
gf doc all-in-one-cfg->json
```

## 函数分类索引

### 链节点记录 `<chain-node>`

- 构造：`make-chain-node`
- 谓词：`chain-node?`
- 属性访问：`chain-node-provider`、`chain-node-model`、`chain-node-thinking-level`
- JSON 转换：`chain-node->json`、`json->chain-node`

### 链定义记录 `<chain-def>`

- 构造：`make-chain-def`
- 谓词：`chain-def?`
- 属性访问：`chain-def-id`、`chain-def-name`、`chain-def-nodes`
- JSON 转换：`chain-def->json`、`json->chain-def`

### 全局配置记录 `<all-in-one-cfg>`

- 构造：`make-all-in-one-cfg`
- 谓词：`all-in-one-cfg?`
- 属性访问：`all-in-one-cfg-continuous?`、`all-in-one-cfg-max-wait-ms`、`all-in-one-cfg-retry-interval-ms`、`all-in-one-cfg-probe-timeout-ms`、`all-in-one-cfg-chains`
- JSON 转换：`all-in-one-cfg->json`、`json->all-in-one-cfg`

## 行为契约

- 一 record 一文件：record 定义、构造器、访问器与 `xxx->json` / `json->xxx` 与 record 同文件；
- 名称与历史公开接口完全一致，调用方无需改动；
- 不新增 `%make-*` 兼容别名、setter 或 `xxx->alist` / `xxx-from-alist`；
- `types` 不依赖 loader / provider / request，也不依赖调度、TUI 或请求门面。
