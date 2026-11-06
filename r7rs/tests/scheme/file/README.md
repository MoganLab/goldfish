# (scheme file) 模块函数分类索引

R7RS `scheme file` 提供了文件输入输出和文件管理的基本操作。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc scheme/file "函数名"
```

## 函数分类索引

### 一、文件端口操作 (R7RS 6.13.1 Ports)
- `open-input-file` - 以输入模式打开文本文件
- `open-output-file` - 以输出模式打开文本文件
- `open-binary-input-file` - 以二进制输入模式打开文件
- `open-binary-output-file` - 以二进制输出模式打开文件
- `call-with-input-file` - 打开文件供输入并调用过程，完成后自动关闭
- `call-with-output-file` - 打开文件供输出并调用过程，完成后自动关闭
- `with-input-from-file` - 将当前输入端口重定向到文件并执行过程
- `with-output-to-file` - 将当前输出端口重定向到文件并执行过程

### 二、文件系统操作 (R7RS 6.13.3 File system)
- `file-exists?` - 检查文件或目录是否存在
- `delete-file` - 删除指定文件
