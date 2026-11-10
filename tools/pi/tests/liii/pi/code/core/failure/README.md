# (liii pi code core failure) 模块函数分类索引

`(liii pi code core failure)` 提供子进程失败诊断与工具错误封装能力。

使用时导入：

```scheme
(import (liii pi code core failure))
```

## 查看函数文档

```bash
gf doc pi-command-failure-detail
gf doc pi-command-error
```

## 函数分类索引

### 失败诊断
- `pi-command-failure-detail` - 提取子进程失败的诊断文本
- `pi-command-error` - 将子进程失败转换为工具调用错误
