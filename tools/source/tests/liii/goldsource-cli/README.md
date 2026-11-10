# (liii goldsource-cli) 模块函数索引

`(liii goldsource-cli)` 提供 `gf source` 命令行流程执行与帮助信息输出。

使用时导入：

```scheme
(import (liii goldsource-cli))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldsource-cli "函数名"
```

## 函数分类索引

### 命令行执行
- `run-goldsource` - 执行 `gf source` 主流程，输出帮助或打印指定库源码
