# (liii goldfmt format writer) 模块函数索引

`(liii goldfmt format writer)` 提供 goldfmt 格式化阶段的输出能力：带位置追踪的 writer、单行内联候选文本计算、首行子节点选择以及各类节点的内联输出。

使用时导入：

```scheme
(import (liii goldfmt format writer))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfmt/format/writer "函数名"
```

## 函数分类索引

### writer 基础
- `make-writer` - 创建带位置追踪的输出 writer
- `writer-line` - 获取 writer 当前所在的行号
- `writer-column` - 获取 writer 当前所在的列号
- `writer-result` - 读取 writer 已输出的全部文本

### 文本输出
- `emit-string!` - 输出文本并更新 writer 的行号与列号
- `emit-newline!` - 输出换行符，行号加一、列号归零
- `emit-spaces!` - 输出 n 个空格，列号相应增加
- `emit-comment!` - 将注释节点写入 writer
- `emit-atom!` - 将 atom 节点写入 writer
- `emit-inline!` - 将可内联节点写入 writer

### 内联判定与文本生成
- `format-inline` - 将 node 转成单行候选字符串
- `try-inline` - 尝试将 node 按单行形式输出
- `can-inline?` - 判断一个 node 是否可以按单行形式输出
- `format-inline-atom-or-quote` - 将 atom 或 quote 形式的值内联格式化为文本
- `format-inline-atom-or-quote-at` - 将 atom 或 quote 形式的值内联格式化，从指定缩进列开始计量

### 首行选择与缩进
- `select-first-line-children` - 选择 env 首行可内联摆放的子节点对列表
- `selected-child` - 获取选中项中的子节点
- `selected-column` - 获取选中项的目标列号
- `selected-count` - 统计选中项的数量
- `next-line-child-indent` - 计算首行选中项之后子节点的缩进列

### 定位信息
- `positioned-atom` - 为 atom 节点生成带缩进与行号定位信息的拷贝
- `positioned-env` - 为 env 节点生成带定位信息的拷贝
