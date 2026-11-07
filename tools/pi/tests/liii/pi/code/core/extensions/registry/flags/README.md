# (liii pi code core extensions registry flags) 模块函数分类索引

`(liii pi code core extensions registry flags)` 提供扩展命令行标志定义的规范化、注册与查找能力。

使用时导入：

```scheme
(import (liii pi code core extensions registry flags))
```

## 查看函数文档

```bash
gf doc normalize-flag-def
gf doc register-flag!
gf doc find-extension-flag
```

## 函数分类索引

### 标志定义注册
- `normalize-flag-def` - 将标志定义统一规范化为扩展标志定义记录
- `register-flag!` - 向注册表注册命令行标志定义
- `extension-register-flag!` - 向注册表注册命令行标志定义（register-flag! 的别名）
- `find-extension-flag` - 按标志名或别名查找注册表中已注册的标志
- `extension-registry-flags-list` - 列出注册表中已注册的全部命令行标志
