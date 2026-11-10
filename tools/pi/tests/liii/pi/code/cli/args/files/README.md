# (liii pi code cli args files) 模块函数分类索引

`(liii pi code cli args files)` 提供命令行文件参数的处理与提示词文件块格式化能力。

使用时导入：

```scheme
(import (liii pi code cli args files))
```

## 查看函数文档

```bash
gf doc format-file-block
gf doc process-file-arguments
```

## 函数分类索引

### 文件参数处理
- `format-file-block` - 将文件内容格式化为注入提示词的文件块
- `process-file-arguments` - 处理命令行中的文件参数，展开目录与 @file 引用
