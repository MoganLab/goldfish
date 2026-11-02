# (liii goldcheck) 模块函数分类索引

Goldfish Scheme 代码规范检查器顶层入口，整合各检查规则、命令行解析、缓存与违规报告输出。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、命令行解析与主入口
- `main` - 运行 check 命令行工具主入口
- `make-check-arg-parser` - 构造 check 工具命令行参数解析器

### 二、全量与增量规则检查
- `check-files` - 批量执行全部启用的规则检查
- `check-file` - 对单个源文件执行各项常规语法与结构规则检查

### 三、子模块重导出函数
以下函数由各专用子模块实现并由顶层库重导出，详见各子模块导览：
- `(liii goldcheck rules)`: `check-file-lines`, `check-tree-depth`, `format-violation`, `display-help`
- `(liii goldcheck no-glue)`: `check-no-glue`
- `(liii goldcheck type-hint)`: `check-type-hint`, `check-type-hint-all`, `scan-hint-libs`
- `(liii goldcheck report)`: `violation-report-lines`, `violation-display-hint`
- `(liii goldcheck unit-test)`: `check-unit-test`
