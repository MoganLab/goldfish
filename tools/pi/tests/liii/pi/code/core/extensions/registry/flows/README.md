# (liii pi code core extensions registry flows) 模块函数分类索引

`(liii pi code core extensions registry flows)` 提供扩展流程定义、参数校验器与配置增强器的注册与查找能力。

使用时导入：

```scheme
(import (liii pi code core extensions registry flows))
```

## 查看函数文档

```bash
gf doc normalize-flow-def
gf doc register-flow!
gf doc register-validator!
gf doc register-config-augmenter!
```

## 函数分类索引

### 流程定义注册
- `normalize-flow-def` - 将流程定义统一规范化为扩展流程定义记录
- `register-flow!` - 向注册表注册流程定义
- `extension-register-flow!` - 向注册表注册流程定义（register-flow! 的别名）
- `find-extension-flow` - 按流程名查找注册表中已注册的流程
- `extension-registry-flows-list` - 列出注册表中已注册的全部流程

### 参数校验器
- `register-validator!` - 向注册表追加参数校验过程
- `extension-register-validator!` - 向注册表追加参数校验过程（register-validator! 的别名）
- `extension-registry-validators-list` - 列出注册表中已注册的全部参数校验过程

### 配置增强器
- `register-config-augmenter!` - 向注册表追加配置增强过程
- `extension-registry-augmenters-list` - 列出注册表中已注册的全部配置增强过程
