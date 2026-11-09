# (srfi srfi-166 columnar) 模块函数分类索引

`(srfi srfi-166 columnar)` 为 SRFI-166 格式化组合子提供
多列排版能力：列对齐、表格、自动折行、两端对齐、行号与
按行读取文件。`(liii format columnar)` 以 Goldfish 命名
重导出本库全部函数。

## 常见用法示例

```scheme
(import (srfi srfi-166 base)
        (srfi srfi-166 columnar))

(show #f (columnar 4 'right (each "ab") " " (each "cd"))) ; => "  ab cd\n"
(show #f (with ((width 8)) (wrapped "hello world")))      ; => "hello\nworld"
(show #f (with ((width 20))
           (tabular (each "a\nbc\n") " | " (each "1\n22\n"))))
; => "a  | 1 \nbc | 22\n"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-166/columnar "函数名"
```

## 函数分类索引

### 一、多列与表格
- `columnar` - 多列并排排版，支持列宽与对齐方式
- `tabular` - 按行对齐的表格排版
- `line-numbers` - 输出递增的行号列

### 二、折行与对齐
- `wrapped` - 按单词边界折行
- `wrapped/list` - 折行已切分好的词表
- `wrapped/char` - 按字符折行，可切断单词
- `justified` - 两端对齐铺满宽度

### 三、内容来源
- `from-file` - 逐行输出文本文件内容

## 错误处理约定

- 折行宽度取状态变量 width，未显式设置时为默认视口宽度
- `from-file` 打开不存在的文件时抛出底层文件错误
