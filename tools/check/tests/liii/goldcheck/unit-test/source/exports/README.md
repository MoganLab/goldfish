# (liii goldcheck unit-test source exports) 模块函数分类索引

库导出名提取：仅关心 `(export ...)` 声明的对外名字，不区分定义与重导出。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、导出名提取
- `extract-library-exports` - 提取库的全部导出名（含定义与重导出）
- `extract-library-defined-exports` - 提取库中由自身定义的导出名
- `extract-library-reexported-entries` - 提取库中重导出的条目（外文名 . 内部名）
