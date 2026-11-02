# (liii goldcheck unit-test source reexport) 模块函数分类索引

重导出符号单元测试归属检查，负责识别并提示将非本库实现的测试移动至定义库目录。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、基础路径与定义库查找
- `find-base-for-source-path` - 查找源码文件所在的库基准目录
- `find-defining-library-test-dir` - 查找符号定义库对应的测试目录
- `find-imported-export-for-stem` - 根据测试名主干查找对应的导入导出符号及目标目录

### 二、重导出单元测试检查
- `check-reexported-tests` - 检查测试目录下是否存在重导出符号的单元测试
- `check-reexported-test-file` - 检查单个单元测试文件是否属于非本库实现的重导出函数

### 三、钩子与依赖注入
- `set-exported-name-to-test-stem-fn!` - 设置符号名转测试名主干的函数
- `set-find-library-source-fn!` - 设置根据测试目录查找库源码路径的函数
