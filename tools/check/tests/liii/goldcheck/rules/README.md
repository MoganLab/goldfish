# (liii goldcheck rules) 模块函数分类索引

`(liii goldcheck rules)` 提供了 `gf check` 工具的核心检查规则、违规格式化与帮助展示功能。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/goldcheck/rules "函数名"
```

## 函数分类索引

### 一、代码度量规则
- `check-file-lines` - 统计单个文件的物理代码行数并校验行数上限
- `check-tree-depth` - 逐顶级表达式度量语法树嵌套深度并校验深度上限
- `check-no-crlf` - 检测文件是否包含 CRLF 换行符
- `check-unit-test` - 检测单元测试文件格式规范（文档置顶、标准小节、尾部 check-report 等）
- `check-unit-test-dirs` - 检查测试文件或源码文件所属测试目录规范（包含 README.md、无废弃单测、覆盖导出函数）

### 二、违规报告与帮助信息
- `format-violation` - 将单条违规记录格式化为统一的终端报告字符串
- `display-help` - 输出 `gf check` 命令的完整帮助说明文本
