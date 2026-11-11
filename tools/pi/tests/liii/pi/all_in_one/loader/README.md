# (liii pi all_in_one loader) 模块函数分类索引

`(liii pi all_in_one loader)` 负责加载并校验 `~/.gf/agent/all-in-one.json` 配置文件，将其解析为内存中的全局配置 `<all-in-one-cfg>` 及链与节点结构体。

使用时导入：

```scheme
(import (liii pi all_in_one loader))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc resolve-all-in-one-config-path
gf doc all-in-one-load-config
```

## 函数分类索引

- `resolve-all-in-one-config-path` - 解析 all-in-one.json 文件的完整路径
- `all-in-one-load-config` - 加载并校验 all-in-one.json，返回配置记录
