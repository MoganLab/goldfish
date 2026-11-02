# (liii goldcheck unit-test source) 模块函数分类索引

`(liii goldcheck unit-test source)` 提供了源码与测试目录/文件之间的双向推断、函数名与测试文件名转换、库导出符号提取与测试覆盖检查功能。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/goldcheck/unit-test/source "函数名"
```

## 函数分类索引

### 一、命名与测试名转换
- `exported-name->test-stem` - 将 Scheme 导出过程名转换为规范的单测文件名主干

### 二、库结构与双向定位
- `file-defines-library?` - 判断指定 Scheme 源码文件是否包含 define-library 声明
- `find-test-dir-for-library-source` - 根据库源码文件路径自动推断其对应的单元测试目录
- `find-library-source-for-test-dir` - 根据测试目录路径反向查找其对应的库源码实现文件

### 三、导出符号提取与单测覆盖校验
- `extract-library-exports` - 从库源码文件中解析提取全部导出标识符
- `check-missing-exported-tests` - 检查测试目录下是否缺失对应库导出函数的单元测试
