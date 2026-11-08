# (srfi srfi-166 color) 模块函数分类索引

`(srfi srfi-166 color)` 提供 SRFI-166 格式化组合子的终端着色能力：
前景色、背景色、文本修饰与自定义 RGB 颜色，
输出的格式化对象可嵌套组合并在结束时恢复先前状态。

## 常见用法示例

```scheme
(import (srfi srfi-166 base)
        (srfi srfi-166 color))

(show #f (as-red "red"))                     ; => "\x1b[31mred\x1b[39m"
(show #f (on-blue (as-yellow "abc") "def"))  ; 前景/背景可嵌套
(show #f (as-true-color 138 190 183 "gf"))   ; 24-bit 真彩色
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```

## 函数分类索引

### 一、前景色
- `as-red` / `as-blue` / `as-green` / `as-cyan` - 标准前景色组合子
- `as-yellow` / `as-magenta` / `as-white` / `as-black` - 标准前景色组合子

### 二、背景色
- `on-red` / `on-blue` / `on-green` / `on-cyan` - 标准背景色组合子
- `on-yellow` / `on-magenta` / `on-white` / `on-black` - 标准背景色组合子

### 三、文本修饰
- `as-bold` - 粗体
- `as-italic` - 斜体
- `as-underline` - 下划线

### 四、自定义颜色
- `as-color` / `on-color` - 6x6x6 调色板（各级别 0~5）
- `as-true-color` / `on-true-color` - 24-bit 真彩色（各分量 0~255）

## 错误处理约定

- 颜色级别或 RGB 分量超出范围时抛出 `error`
