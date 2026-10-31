(import (liii check)
        (liii tui text-width))

(check-set-mode! 'report-failed)

;; display-width
;; 计算字符串在终端中的视觉显示列宽。
;;
;; 语法
;; ----
;; (display-width str)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;;
;; 返回值
;; ----
;; integer
;; 视觉显示占用的终端列数。
;;
;; 描述
;; ----
;; display-width 是纯函数：
;; - ANSI CSI/SGR 控制序列显示宽度计为 0；
;; - ASCII 可见字符显示宽度计为 1；
;; - CJK 表意字符、全角标点、常见 Emoji 字符显示宽度计为 2；
;; - 零宽字符与组合标音字符显示宽度计为 0。
;;
;; 错误处理
;; ----
;; type-error 当 str 不是字符串时。

;; 空字符串
(check (display-width "") => 0)

;; ASCII 纯文本
(check (display-width "a") => 1)
(check (display-width "hello") => 5)
(check (display-width "hello, world!") => 13)

;; CJK 表意汉字与全角标点符号（每个 2 列）
(check (display-width "你") => 2)
(check (display-width "你好") => 4)
(check (display-width "金鱼Scheme") => 10)
(check (display-width "【金鱼】！") => 10)

;; 常见 Emoji 字符（每个 2 列）
(check (display-width "😊") => 2)
(check (display-width "🚀") => 2)
(check (display-width "Hi 😊!") => 6)
(check (display-width "🚀金鱼") => 6)

;; 零宽字符（U+200B 零宽空格，U+200D ZWJ）
(check (display-width (utf8->string #u8(226 128 139))) => 0)
(check (display-width (string-append "a" (utf8->string #u8(226 128 139)) "b")) => 2)

;; ANSI 序列（不占用显示列宽）
(check (display-width "\x1b[0m") => 0)
(check (display-width "\x1b[31mred\x1b[0m") => 3)
(check (display-width "\x1b[1;32mhello, world!\x1b[0m") => 13)
(check (display-width "\x1b[33m金鱼\x1b[0m") => 4)
(check (display-width "\x1b[1m\x1b[34m🚀金鱼\x1b[0m") => 6)

;; OSC 序列计 0 宽（终端标题、背景色查询）
(check (display-width "\x1b]0;title\x07hi") => 2)
(check (display-width "\x1b]11;?\x1b\\ok") => 2)

;; 非法 UTF-8 字节：容错重算，与清理后文本的渲染宽度一致（U+FFFD 宽 1），
;; 不再因 string->utf8 校验失败抛 value-error（防御深度：入口未清理也能测宽）
(check (display-width (string-append "你" (byte-vector->string #u8(229 165)))) => 3)
(check (display-width (byte-vector->string #u8(224 128 128))) => 3)
(check (display-width (string-append "ab" (byte-vector->string #u8(128)))) => 3)

;; 非法字节与 ANSI 序列共存
(check (display-width (string-append "\x1b[31m" (byte-vector->string #u8(255)) "\x1b[0m")) => 1)

;; 类型错误检查
(check-catch 'type-error (display-width 123))
(check-catch 'type-error (display-width 'sym))
(check-catch 'type-error (display-width #f))

(check-report)
