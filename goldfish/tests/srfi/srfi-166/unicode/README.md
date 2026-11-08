# (srfi srfi-166 unicode) 模块函数分类索引

`(srfi srfi-166 unicode)` 为 SRFI-166 格式化组合子提供
Unicode 显示宽度感知能力：按终端列宽计算与截取字符串、
保留 ANSI 转义序列，以及大小写转换组合子。
`(liii format unicode)` 以 Goldfish 命名重导出本库全部函数。

## 常见用法示例

```scheme
(import (srfi srfi-166 base)
        (srfi srfi-166 unicode))

(string-terminal-width "日本語")            ; => 6
(substring-terminal-width "ａｂｃ" 0 4)     ; => "ａｂ"
(show #f (upcased "abc"))                   ; => "ABC"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```

## 函数分类索引

### 一、显示宽度
- `string-terminal-width` - 计算字符串的终端显示列宽
- `string-terminal-width/wide` - 歧义字符按 2 列计算的宽度
- `substring-terminal-width` - 按显示宽度区间截取子串
- `substring-terminal-width/wide` - 歧义字符按 2 列的截取
- `substring-terminal-preserve` - 提取字符串中的 ANSI 转义序列

### 二、格式化组合子
- `terminal-aware` - 使内部宽度敏感操作感知 Unicode 显示宽度
- `upcased` - 输出统一大写化
- `downcased` - 输出统一小写化

## 错误处理约定

- 索引越界等参数错误抛出底层范围错误
