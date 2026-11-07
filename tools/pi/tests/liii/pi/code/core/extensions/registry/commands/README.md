# (liii pi code core extensions registry commands) 模块函数分类索引

`(liii pi code core extensions registry commands)` 提供扩展命令定义的规范化、注册与查找能力。

使用时导入：

```scheme
(import (liii pi code core extensions registry commands))
```

## 查看函数文档

```bash
gf doc normalize-command-def
gf doc register-command!
gf doc find-extension-command
```

## 函数分类索引

### 命令定义注册
- `normalize-command-def` - 将命令定义统一规范化为扩展命令定义记录
- `register-command!` - 向注册表注册命令定义
- `extension-register-command!` - 向注册表注册命令定义（register-command! 的别名）
- `find-extension-command` - 按命令名查找注册表中已注册的命令
- `extension-registry-commands-list` - 列出注册表中已注册的全部命令
