# (liii pi code core extensions registry tools) 模块函数分类索引

`(liii pi code core extensions registry tools)` 提供扩展工具定义的规范化、注册与查找能力。

使用时导入：

```scheme
(import (liii pi code core extensions registry tools))
```

## 查看函数文档

```bash
gf doc normalize-tool-def
gf doc register-tool!
gf doc find-extension-tool
```

## 函数分类索引

### 工具定义注册
- `normalize-tool-def` - 将工具定义统一规范化为扩展工具定义记录
- `register-tool!` - 向注册表注册工具定义，同名工具按名称覆盖
- `extension-register-tool!` - 向注册表注册工具定义（register-tool! 的别名）
- `find-extension-tool` - 按名称查找注册表中已注册的工具
- `extension-registry-tools-list` - 列出注册表中已注册的全部工具
- `extension-registry-default-tool-names` - 列出声明为默认启用的工具名
