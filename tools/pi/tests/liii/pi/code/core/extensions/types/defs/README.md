# (liii pi code core extensions types defs) 模块函数分类索引

`(liii pi code core extensions types defs)` 提供扩展工具、命令行标志、命令与流程四种定义记录类型及其访问器。

使用时导入：

```scheme
(import (liii pi code core extensions types defs))
```

## 查看函数文档

```bash
gf doc make-extension-tool-def
gf doc make-extension-flag-def
gf doc make-extension-command-def
gf doc make-extension-flow-def
```

## 函数分类索引

### 工具定义
- `make-extension-tool-def` - 创建扩展工具定义记录
- `extension-tool-def?` - 判断对象是否为扩展工具定义记录
- `extension-tool-def-name` - 读取工具定义中的名称字符串
- `extension-tool-def-description` - 读取工具定义中的描述字符串
- `extension-tool-def-parameters` - 读取工具定义中的参数模式
- `extension-tool-def-execute` - 读取工具定义中的执行过程
- `extension-tool-def-snippet` - 读取工具定义中的使用片段字符串
- `extension-tool-def-guidelines` - 读取工具定义中的使用指引列表
- `extension-tool-def-default-active?` - 读取工具定义中的默认启用状态

### 标志定义
- `make-extension-flag-def` - 创建扩展命令行标志定义记录
- `extension-flag-def?` - 判断对象是否为扩展标志定义记录
- `extension-flag-def-flag` - 读取标志定义中的命令行标志字符串
- `extension-flag-def-description` - 读取标志定义中的描述字符串
- `extension-flag-def-handler` - 读取标志定义中的处理过程
- `extension-flag-def-type` - 读取标志定义中的取值类型符号
- `extension-flag-def-default` - 读取标志定义中的默认值
- `extension-flag-def-key` - 读取标志定义中的配置键符号
- `extension-flag-def-aliases` - 读取标志定义中的别名列表
- `extension-flag-def-takes-arg?` - 读取标志定义中的是否需要参数
- `extension-flag-def-validator` - 读取标志定义中的校验过程

### 命令定义
- `make-extension-command-def` - 创建扩展命令定义记录
- `extension-command-def?` - 判断对象是否为扩展命令定义记录
- `extension-command-def-name` - 读取命令定义中的命令名字符串
- `extension-command-def-description` - 读取命令定义中的描述字符串
- `extension-command-def-handler` - 读取命令定义中的处理过程
- `extension-command-def-params` - 读取命令定义中的参数列表

### 流程定义
- `make-extension-flow-def` - 创建扩展流程定义记录
- `extension-flow-def?` - 判断对象是否为扩展流程定义记录
- `extension-flow-def-name` - 读取流程定义中的流程名字符串或符号
- `extension-flow-def-predicate` - 读取流程定义中的判定过程
- `extension-flow-def-handler` - 读取流程定义中的处理过程
- `extension-flow-def-local?` - 读取流程定义中的是否为本地流程
- `extension-flow-def-priority` - 读取流程定义中的优先级数值
