# (liii tempfile) 模块函数分类索引

`(liii tempfile)` 提供临时文件与临时目录的原子创建能力，适合测试夹具、导出中间产物与一次性工作目录。

## 查看函数文档

```bash
gf doc liii/tempfile
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/tempfile "make-temp-dir"
```

## 函数分类索引

### 一、临时目录
- `make-temp-dir` - 原子独占创建临时目录，返回目录绝对路径
- `with-temp-dir` - 作用域安全临时目录执行，退出作用域时递归删除整个目录树

### 二、临时文件
- `make-temp-file` - 原子独占创建临时文件，返回已打开的输出端口与文件绝对路径
- `with-temp-file` - 作用域安全临时文件执行，退出作用域时自动删除临时文件
