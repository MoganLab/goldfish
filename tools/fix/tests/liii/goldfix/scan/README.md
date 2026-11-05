# (liii goldfix scan) 模块函数索引

`(liii goldfix scan)` 提供括号修正所需的词法扫描能力：字符分类、字符串/注释/字符字面量等记号的边界查找。

使用时导入：

```scheme
(import (liii goldfix scan))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfix/scan "函数名"
```

## 函数分类索引

### 字符分类
- `whitespace-char?` - 判断字符是否为空白字符
- `delimiter-char?` - 判断字符是否为分隔符（空白、括号、分号、双引号）

### 边界查找
- `string-prefix-at?` - 判断字符串在指定位置是否具有给定前缀
- `find-string-end` - 查找字符串字面量的结束位置
- `find-bar-symbol-end` - 查找竖线符号 |...| 的结束位置
- `find-block-comment-end` - 查找块注释 #|...|# 的结束位置（支持嵌套）
- `find-raw-string-end` - 查找原始字符串 #"..."# 的结束位置
- `find-char-end` - 查找字符字面量 #\x 的结束位置
- `find-line-comment-end` - 查找行注释的结束位置
- `find-other-end` - 查找普通记号的结束位置
