(import (liii check)
        (liii tui text-width))

(check-set-mode! 'report-failed)

;; strip-ansi
;; 剔除字符串中的 ANSI 转义序列（包括 SGR 颜色序列和 CSI 控制序列）。
;;
;; 语法
;; ----
;; (strip-ansi str)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;;
;; 返回值
;; ----
;; string
;; 移除了所有 ANSI 转义序列的纯文本字符串。
;;
;; 描述
;; ----
;; strip-ansi 是纯函数，扫描输入字符串并去除形如 ESC [ ... <final-char> 的
;; CSI/SGR 控制序列，例如颜色代码 \x1b[31;1m 与重置代码 \x1b[0m。
;; 普通文本和空字符串不受影响。
;;
;; 错误处理
;; ----
;; type-error 当 str 不是字符串时。

;; 空字符串
(check (strip-ansi "") => "")

;; 普通纯文本
(check (strip-ansi "hello world") => "hello world")
(check (strip-ansi "你好，世界") => "你好，世界")

;; 基础 ANSI SGR 颜色代码
(check (strip-ansi "\x1b[31mred\x1b[0m") => "red")
(check (strip-ansi "\x1b[1;32mbold green\x1b[0m") => "bold green")

;; 多个样式嵌套或连续
(check (strip-ansi "\x1b[1m\x1b[34mblue bold\x1b[39m\x1b[22m") => "blue bold")

;; 256 色和 24 位真彩色
(check (strip-ansi "\x1b[38;5;196mcolor256\x1b[0m") => "color256")
(check (strip-ansi "\x1b[38;2;255;100;50mtruecolor\x1b[0m") => "truecolor")

;; 中文与 ANSI 混合
(check (strip-ansi "\x1b[33m金鱼\x1b[0mScheme") => "金鱼Scheme")

;; CSI 光标控制或清除序列
(check (strip-ansi "\x1b[2J\x1b[HHello") => "Hello")

;; OSC 序列（终端标题设置，BEL 终结）
(check (strip-ansi "\x1b]0;title\x07hello") => "hello")

;; OSC 序列（ESC \\ 终结，如背景色查询 OSC 11、超链接）
(check (strip-ansi "\x1b]11;?\x1b\\ok") => "ok")
(check (strip-ansi "a\x1b]8;;http://x\x1b\\link\x1b]8;;\x1b\\b") => "alinkb")

;; 不完整 OSC（无终结符）：保守保留原文，等待后续块补全
(check (strip-ansi "\x1b]0;tit") => "\x1b]0;tit")

;; OSC 与 CSI 混合
(check (strip-ansi "\x1b]0;t\x07\x1b[31mred\x1b[0m") => "red")

;; 纯转义序列
(check (strip-ansi "\x1b[0m") => "")
(check (strip-ansi "\x1b[31m\x1b[0m") => "")

;; 非法 UTF-8 字节：bash 命令输出可能含被截断的多字节序列（如被截断的
;; “好”只剩 e5 a5）或二进制垃圾；与上游 JS 字符串层一致，替换为 U+FFFD
;; 而不是抛 value-error 使整个输出丢失
;; （用 byte-vector->string 构造含非法字节的原始字符串）
(check (strip-ansi (string-append "你好" (byte-vector->string #u8(229 165)) "..."))
  => (string-append "你好" (utf8->string #u8(239 191 189)) "..."))

;; 非法首字节（0xFF 孤立字节）与 ANSI 序列共存
(check (strip-ansi (string-append "\x1b[31ma" (byte-vector->string #u8(255)) "b\x1b[0m"))
  => (string-append "a" (utf8->string #u8(239 191 189)) "b"))

;; 非法尾字节：合法序列后跟孤立续字节
(check (strip-ansi (string-append "a" (byte-vector->string #u8(128)) "b"))
  => (string-append "a" (utf8->string #u8(239 191 189)) "b"))

;; 过长编码（E0 80 80）与 ANSI 序列共存：按 WHATWG 替换为 3 个 U+FFFD
(check (strip-ansi (string-append "\x1b[31m" (byte-vector->string #u8(224 128 128)) "\x1b[0m"))
  =>
  (string-append (utf8->string #u8(239 191 189))
    (utf8->string #u8(239 191 189))
    (utf8->string #u8(239 191 189))
  ) ;string-append
) ;check

;; 类型错误检查
(check-catch 'type-error (strip-ansi 123))
(check-catch 'type-error (strip-ansi 'symbol))
(check-catch 'type-error (strip-ansi #f))

(check-report)
