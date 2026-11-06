# (liii goldtool changed) 模块函数分类索引

`(liii goldtool changed)` 基于 git diff 计算「自某个版本以来变更了哪些文件」，
为 `gf fmt`、`gf fix`、`gf check`、`gf test` 等工具的增量模式提供统一的变更集合。

使用时导入：

```scheme
(import (liii goldtool changed))
```

## 常见用法示例

```scheme
;; 示例1：列出自 main 分支以来变更的所有文件
(changed-files-since "main")

;; 示例2：只列出仍然存在的变更文件（跳过已删除的文件）
(changed-existing-files-since "main")

;; 示例3：只列出变更的 Scheme 源文件
(changed-scheme-files-since "main")
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldtool/changed "函数名"
```

## 函数分类索引

### 一、变更文件查询

- `changed-files-since` - 列出自某版本以来变更的文件路径
- `changed-existing-files-since` - 列出仍然存在的变更文件路径
- `changed-scheme-files-since` - 列出变更的 Scheme 源文件路径
