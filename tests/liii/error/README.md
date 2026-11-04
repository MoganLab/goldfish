# (liii error) 模块函数分类索引

`(liii error)` 提供 Goldfish Scheme 核心错误类型构造、运行时异常捕获格式化、源码位置提取与未绑定符号导入提示（Hint）功能。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/error "函数名"
```

## 函数分类索引

### 一、异常类型构造
- `???` - 抛出未知或未实现标记错误
- `os-error` - 抛出操作系统级错误
- `process-not-found-error` - 抛出进程未找到错误
- `file-not-found-error` - 抛出文件未找到错误
- `file-error` - 抛出通用文件操作错误
- `not-a-directory-error` - 抛出路径非目录错误
- `file-exists-error` - 抛出文件已存在错误
- `permission-denied-error` - 抛出权限不足错误
- `timeout-error` - 抛出超时错误
- `type-error` - 抛出类型错误
- `type-error?` - 判定错误标签是否属于类型错误
- `key-error` - 抛出键不存在错误
- `value-error` - 抛出数值或取值不合法错误
- `index-error` - 抛出索引越界错误

### 二、异常信息格式化与提示
- `catch-error->string` - 综合格式化 catch 捕获到的异常信息，包含消息展开、位置与提示
- `extract-unbound-variable` - 从未绑定变量异常参数中提取缺失的符号名称
- `extract-error-location` - 从异常表达式或错误上下文中提取源码文件与行号
- `format-error-location` - 将文件与行号格式化为源码定位后缀
- `unbound-variable-hint` - 基于 load-path 扫描结果生成库导入建议
