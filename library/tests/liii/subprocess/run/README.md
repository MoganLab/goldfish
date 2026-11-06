# (liii subprocess run) 模块函数分类索引

`(liii subprocess run)` 提供同步子进程执行接口，以及命令组合器使用的参数拆分和多返回值辅助函数。

## 查看函数文档

```bash
gf doc liii/subprocess/run
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/subprocess/run "run-either"
```

## 函数分类索引

### 一、命令执行
- `run` - 执行命令并返回退出码
- `run-either` - 成功时返回标准输出的 Right，失败时返回错误记录的 Left

### 二、内部辅助函数
- `%run-values` - 以三个值返回标准输出、标准错误和退出码
- `%symbol-keyword?` - 识别以冒号开头的选项关键字符号
- `%split-args` - 拆分命令列表和尾部关键字选项
