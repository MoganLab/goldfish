# (liii goldcheck unit-test file) 模块函数分类索引

单测试文件内容规范检查，包含按行读取、空行检索、文档结构与末尾 report 检查等。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、文件内容读取与行索引
- `read-all-lines` - 按行读取文件全部内容
- `find-first-non-empty-index` - 查找首个非空行下标
- `find-last-non-empty-index` - 查找最后一个非空行下标

### 二、测试规范检查
- `check-report-tail` - 检查测试文件末尾是否为 (check-report) 及上方是否有空行
- `parent->lib-name` - 将测试目录相对路径转换为库名形式
- `check-unit-test-file-content` - 检查单个测试文件的内容规范
- `check-unit-test` - 检查测试文件是否符合规范并检测废弃单测试文件方案
- `unit-test-file?` - 判断指定文件是否为应遵循单元测试规范的测试文件
