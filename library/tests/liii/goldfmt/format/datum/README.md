# (liii goldfmt format datum) 模块函数索引

`(liii goldfmt format datum)` 提供 reader 路径的 datum 辅助能力：识别内部形式标记（原始字符串、换行、注释）、计算首行选中项信息、fill 布局判定以及结果文本的拼装。

使用时导入：

```scheme
(import (liii goldfmt format datum))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfmt/format/datum "函数名"
```

## 函数分类索引

### 内部形式判定
- `single-arg-symbol-form?` - 判断 value 是否为 (name x) 形式的单参数表单
- `quote-syntax-form?` - 判断 value 是否为 #_quote 语法对象包裹的单参数表单
- `raw-string-datum?` - 判断 datum 是否为原始字符串字面量的内部形式
- `newline-marker-datum?` - 判断 datum 是否为换行标记的内部形式
- `comment-datum?` - 判断 datum 是否为注释标记的内部形式
- `reader-datum-contains-newline-marker?` - 判断 datum 中是否包含换行或注释标记

### 表单头与结构
- `reader-head-name` - 读取表单头符号的名称
- `reader-rule-head?` - 判断表单头是否可以作为排版规则的名称
- `reader-form-like?` - 判断 datum 是否为列表形式的表单
- `reader-newlines` - 将换行数量转换为换行符串

### 首行选中项
- `reader-selected-item` - 读取选中项中的表单内容
- `reader-selected-column` - 读取选中项中的目标列号
- `reader-selected-text` - 读取选中项中的文本
- `reader-skip-selected` - 跳过已选中的表单，返回剩余部分
- `reader-first-selected-form-column` - 查找首个选中列表单的目标列号

### 缩进与文本拼装
- `reader-by-first-rest-child-indent` - 按首个剩余子节点计算续行缩进
- `reader-rest-indent` - 按排版规则计算续行缩进
- `reader-append-selected` - 将首行选中的表单追加到结果文本中
- `reader-append-close` - 为结果文本追加闭合括号
- `last-line-column` - 计算文本最后一行的列数

### fill 布局
- `reader-fill-atom?` - 判断元素是否可以参与 fill 布局
- `reader-fill-eligible?` - 判断表单是否可以采用 fill 布局
- `format-reader-pair-fill` - 以 fill 布局格式化表单
- `reader-vector-prefix` - 读取向量字面量的前缀

### keyword 参数
- `reader-keyword-block-info` - 计算可以内联的连续 keyword 参数对信息
