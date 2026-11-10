# (liii pi code core extensions registry) 模块函数分类索引

`(liii pi code core extensions registry)` 聚合重导出扩展注册表操作与各类扩展注册入口。

使用时导入：

```scheme
(import (liii pi code core extensions registry))
```

## 子库导览

- `registry/base/` - 注册表对象解析、当前/默认注册表与初始化器
- `registry/tools/` - 工具定义注册与查找
- `registry/flags/` - 命令行标志定义注册与查找
- `registry/commands/` - 命令定义注册与查找
- `registry/flows/` - 流程定义、参数校验器与配置增强器注册
- `registry/prompt/` - 提示词注入器注册与收集

## 查看函数文档

```bash
gf doc target->registry
gf doc register-tool!
gf doc register-prompt-injector!
```
