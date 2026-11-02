# (liii goldcheck type-hint parse) 模块函数分类索引

define-library 库文件源码 AST 只读解析，提取库名、import 依赖、export 符号及函数 define 定义与参数 arity 信息。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、源码与 S 表达式读取
- `read-all-forms` - 读取文件中所有的顶级 S 表达式形式列表
- `form-line` - 获取 S 表达式对应的源码行号
- `proper-list?` - 判断表达式是否为合法的真列表（非点对列表）
- `find-define-library` - 从 S 表达式列表中查找 define-library 顶级形式

### 二、库结构解析与属性访问
- `parse-library-file` - 解析指定库文件并返回包含库名、行号、导入、导出及定义的 alist
- `lib-name` - 提取库信息中的库名符号列表
- `lib-line` - 提取库信息中的 define-library 所在行号
- `lib-imports` - 提取库信息中的所有导入库名列表
- `lib-exports` - 提取库信息中的所有对外导出符号列表
- `lib-defines` - 提取库信息中在 begin 块内定义的函数或变量记录列表

### 三、Define 记录属性访问
- `def-name` - 提取 define 定义记录中的符号名称
- `def-kind` - 提取 define 定义类型（lambda / case-lambda / rest / value）
- `def-arity` - 提取函数定义的参数数量规格
- `def-line` - 提取 define 定义所在的源码行号
- `def-expr` - 提取 define 定义对应的原始 S 表达式
