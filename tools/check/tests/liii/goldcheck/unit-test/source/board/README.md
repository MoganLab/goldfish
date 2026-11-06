# (liii goldcheck unit-test source board) 模块函数分类索引

板块路径推导，负责在仓库四个板块（r7rs/ goldfish/ library/ tools/）之间
定位库源码文件与库标识符的对应关系。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、板块基准目录与候选源码路径
- `find-base-for-source-path` - 查找源码文件所在的板块基准目录
- `candidate-source-paths` - 按库标识符列出各板块根目录下的候选源码路径
