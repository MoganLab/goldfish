# (liii argparse) 模块函数分类索引

`(liii argparse)` 用于解析命令行参数，支持长选项（`--name`）、短选项（`-n`）、行内取值（`--name=value`）、默认值与布尔开关。

## 查看函数文档

```bash
gf doc liii/argparse
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/argparse "make-argument-parser"
```

## 函数分类索引

### 一、解析器创建
- `make-argument-parser` - 创建命令行参数解析器

## 解析器命令

`make-argument-parser` 返回的解析器过程支持以下命令：

- `:add-argument` / `:add` - 添加参数定义
- `:parse-args` / `:parse` - 解析参数列表（缺省使用 `(cddr (argv))`）
- `:parse-argv` - 解析完整命令行
- `:get-argument` / `:get` - 按名称获取参数值
- `:positionals` / `:get-positionals` - 获取位置参数
- `(parser 'name)` - 按名称取值的快捷写法
