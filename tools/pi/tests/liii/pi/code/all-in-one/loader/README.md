# (liii pi code all-in-one loader) 模块函数分类索引

`(liii pi code all-in-one loader)` 负责 `all-in-one.json` 的路径解析、加载校验、缺省回退与首次初始化。

使用时导入：

```scheme
(import (liii pi code all-in-one loader))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc resolve-all-in-one-config-path
gf doc all-in-one-load-config
gf doc all-in-one-load-or-default-config
gf doc ensure-all-in-one-config!
```

## 函数分类索引

### 路径

- `resolve-all-in-one-config-path` - 解析 `all-in-one.json` 的完整路径（缺省为 `~/.gf/agent/all-in-one.json`）

### 读取

- `all-in-one-load-config` - 加载并校验 `all-in-one.json`，读到有效用户配置才返回记录，否则返回 `#f`

### 缺省回退

- `all-in-one-load-or-default-config` - 统一回退入口：读到有效配置就用它，否则返回默认配置记录，永不返回 `#f`

### 初始化

- `ensure-all-in-one-config!` - 文件不存在时写入默认 `all-in-one.json`；已存在时返回 `#f` 且不覆盖

## 两个读取入口的分工

| 入口 | 语义 | 返回 |
| --- | --- | --- |
| `all-in-one-load-config` | 是否成功读到用户配置 | `(or <all-in-one-cfg> #f)` |
| `all-in-one-load-or-default-config` | 拿一份可直接使用的配置 | `<all-in-one-cfg>`，永不为 `#f` |

请求门面、压缩窗口计算与配置 TUI 使用后者以消除重复的默认构造；需要区分“用户配置是否存在”的调用方（如 `provider` 的可用性判定）使用前者。

## 行为契约

- 加载失败不抛异常：JSON 非法、根节点不是对象、根 `provider` 不是 `all-in-one`、`chains` 非数组，都写告警并返回 `#f`；
- schema 校验失败只写日志，不中断加载；
- 默认文件由 `(liii pi code all-in-one defaults)` 的默认记录序列化得到，不维护第二份手写默认链 JSON，字段与链顺序和默认记录完全一致；
- 首次生成使用原子写；**已存在的用户文件绝不覆盖**；
- 回退路径不写任何文件。
