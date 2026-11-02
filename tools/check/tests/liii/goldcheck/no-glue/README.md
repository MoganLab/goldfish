# (liii goldcheck no-glue) 模块函数分类索引

胶水层使用规范检查，确保 glue 符号仅在库内部使用且不被多个库重复引用或直接导出。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、胶水符号使用规则检查
- `check-no-glue` - 检查单文件中 glue 是否只在 define-library 中使用且未被直接导出
- `check-no-glue-duplicates` - 检查同一 glue 是否被多个 define-library 重复使用
