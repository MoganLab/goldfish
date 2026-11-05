# (liii goldfix batch) 模块函数索引

`(liii goldfix batch)` 提供 gf fix 的批量处理能力：单文件修正、文件列表批处理、目录递归处理以及按版本过滤变更文件。

使用时导入：

```scheme
(import (liii goldfix batch))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfix/batch "函数名"
```

## 函数分类索引

### 单文件处理
- `fix-file-dry-run` - 预览模式：输出修正后的源码，不写回磁盘
- `fix-file-core` - 处理单个文件，返回 cached / updated / balanced / skipped 状态
- `fix-file` - 处理单个文件并启用缓存

### 批量处理
- `fix-file-list` - 批量处理文件列表，返回 (总数 已更新数 命中缓存数)
- `fix-directory` - 递归处理目录下匹配后缀名的文件

### 文件筛选
- `file-extension-match?` - 判断文件名是否以给定后缀名之一结尾
- `fix-changed-since` - 只处理自指定版本以来变更过的文件
