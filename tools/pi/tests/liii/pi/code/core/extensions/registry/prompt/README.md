# (liii pi code core extensions registry prompt) 模块函数分类索引

`(liii pi code core extensions registry prompt)` 提供提示词注入器的注册与收集能力。

使用时导入：

```scheme
(import (liii pi code core extensions registry prompt))
```

## 查看函数文档

```bash
gf doc register-prompt-injector!
gf doc extension-registry-prompt-injectors-list
gf doc extension-registry-collect-prompt-injectors
```

## 函数分类索引

### 提示词注入器
- `register-prompt-injector!` - 向注册表追加提示词注入器
- `extension-register-prompt-injector!` - 向注册表追加提示词注入器（register-prompt-injector! 的别名）
- `extension-registry-prompt-injectors-list` - 列出注册表中已注册的全部提示词注入器
- `extension-registry-collect-prompt-injectors` - 求值并收集注册表中的提示词注入结果
