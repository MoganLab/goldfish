# (liii pi review files) 模块函数分类索引

`(liii pi review files)` 提供代码审查文件的读取、截断与记录封装能力，并聚合重导出 git 变更文件接口。

使用时导入：

```scheme
(import (liii pi review files))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc PI_REVIEW_MAX_FILES
gf doc PI_REVIEW_MAX_FILE_CHARS
gf doc pi-review-file-records
```

## 函数分类索引

### 常量
- `PI_REVIEW_MAX_FILES` - 单次代码审查允许处理的最大文件数量
- `PI_REVIEW_MAX_FILE_CHARS` - 单个审查文件截断的最大字符数上限

### 文件记录
- `pi-review-file-records` - 读取待审查文件内容并检测其编程语言，生成审查文件记录列表
