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
- `display-help` - 显示命令行帮助信息

### 二、全量与增量规则检查
- `check-files` - 批量执行全部启用的规则检查
- `check-file` - 对单个源文件执行各项常规语法与结构规则检查
- `check-file-lines` - 检查单文件行数限制
- `check-tree-depth` - 检查单文件表达式语法树深度
- `check-no-glue` - 检查胶水层使用规范
- `check-type-hint` - 检查单个模块的类型注解合规性
- `check-type-hint-all` - 全局检查类型注解规则
- `scan-hint-libs` - 扫描包含类型注解的库
- `check-unit-test` - 执行单元测试规范检查

### 三、违规格式化与报告输出
- `format-violation` - 将单条违规数据格式化为标准终端报告行
- `violation-report-lines` - 批量格式化违规记录
- `violation-display-hint` - 提取违规相关的类型提示信息
