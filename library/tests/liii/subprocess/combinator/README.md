# (liii subprocess combinator) 模块函数分类索引

`(liii subprocess combinator)` 提供命令组合接口，以 Either 返回执行结果，无需通过 shell 字符串拼接命令。

## 查看函数文档

```bash
gf doc liii/subprocess/combinator
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/subprocess/combinator "run-and"
```

## 函数分类索引

### 一、顺序执行与管道
- `run-and` - 顺序执行命令，任一失败后停止
- `run-or` - 顺序执行命令，任一成功后停止
- `run-sequence` - 执行全部命令并返回最后一个命令的结果
- `run-pipe` - 将前一命令的标准输出作为后一命令的输入

### 二、条件执行
- `run-if` - 根据条件命令的退出码选择执行分支
- `run-when` - 条件命令失败时执行指定命令
