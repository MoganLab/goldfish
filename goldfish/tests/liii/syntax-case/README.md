# (liii syntax-case) 模块函数分类索引

`(liii syntax-case)` 提供 R6RS 风格的过程式卫生宏扩展机制，派生自 Chibi Scheme (chibi syntax-case)，作者 Marc Nieper-Wißkirchen。

## 查看函数文档

使用以下命令查看函数与宏文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/syntax-case "函数名"
```

## 函数分类索引

### 一、模式匹配与转录核心宏
- `syntax-case` - 模式匹配语法对象并执行展开期任意计算过程
- `syntax` - 构建保留词法上下文的语法对象模板
- `quasisyntax` - 语法准引用模板，支持 unsyntax 嵌入
- `unsyntax` - 准引用内部求值注入
- `unsyntax-splicing` - 准引用内部列表平铺注入
- `with-syntax` - 局部模式变量绑定
- `with-ellipsis` - 指定局部自定义省略号关键字

### 二、语法对象与原生数据互转
- `datum->syntax` - 将原生 S 表达式包装为携带词法作用域的语法对象
- `syntax->datum` - 递归剥除语法闭包还原为原生 S 表达式

### 三、标识符操作与谓词
- `free-identifier=?` - 比较两标识符在各自环境中的有效绑定
- `bound-identifier=?` - 比较两标识符是否同名且具有相同绑定身份
- `ellipsis-identifier?` - 判断是否为省略号标识符 ...
- `generate-temporaries` - 生成不冲突的唯一临时标识符列表
- `syntax-violation` - 抛出结构化语法错误异常
