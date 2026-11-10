# (liii pi code utils git) 模块函数分类索引

`(liii pi code utils git)` 提供 Git 变更文件的状态收集、命令行构造与输出解析能力。

使用时导入：

```scheme
(import (liii pi code utils git))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc PI_DEFAULT_MAX_CHANGED_FILES
gf doc pi-changed-files-command
gf doc pi-collect-changed-files
gf doc pi-parse-changed-files
```

## 函数分类索引

### 常量
- `PI_DEFAULT_MAX_CHANGED_FILES` - 默认最大变更文件收集数量

### Git 变更处理
- `pi-changed-files-command` - 构造获取变更文件状态的 git diff 命令字符串
- `pi-parse-changed-files` - 解析 git diff --name-status 输出为变更文件字典列表
- `pi-collect-changed-files` - 收集 git 变更文件列表，支持注入 runner
