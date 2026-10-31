(import (liii check)
        (liii tui text-width))

(check-set-mode! 'report-failed)

;; display-width-prefix
;; 计算字符串前 n 个 Unicode 字符在终端中占用的视觉列宽。
;;
;; 语法
;; ----
;; (display-width-prefix str n)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;;
;; n : integer
;; 参与计算的字符（Unicode 码点）个数，不是字节数。
;;
;; 返回值
;; ----
;; integer
;; 前 n 个字符视觉显示占用的终端列数。
;;
;; 描述
;; ----
;; display-width-prefix 是 display-width 的前缀版本，宽度规则完全一致：
;; - ANSI CSI/SGR 控制序列显示宽度计为 0；
;; - ASCII 可见字符显示宽度计为 1；
;; - CJK 表意字符、全角标点、常见 Emoji 字符显示宽度计为 2；
;; - 零宽字符与组合标音字符显示宽度计为 0。
;;
;; 与 display-width 不同的是 n 以字符计数，因此中文字符只消耗 1 个字符配额，
;; 但仍占用 2 个终端列。常用于把字符位置换算成终端光标列位置。
;;
;; 错误处理
;; ----
;; type-error 当 str 不是字符串，或 n 不是非负整数时。

;; 空字符串与零字符
(check (display-width-prefix "" 0) => 0)
(check (display-width-prefix "hello" 0) => 0)

;; ASCII 纯文本
(check (display-width-prefix "hello" 1) => 1)
(check (display-width-prefix "hello" 3) => 3)
(check (display-width-prefix "hello" 5) => 5)

;; n 超过字符总数时取整个字符串的宽度
(check (display-width-prefix "hello" 10) => 5)

;; CJK 表意汉字（每个字符 1 个配额，占 2 列）
(check (display-width-prefix "你好世界" 1) => 2)
(check (display-width-prefix "你好世界" 2) => 4)
(check (display-width-prefix "你好世界" 4) => 8)

;; 中英混合
(check (display-width-prefix "Hi金鱼" 2) => 2)
(check (display-width-prefix "Hi金鱼" 3) => 4)
(check (display-width-prefix "Hi金鱼" 4) => 6)

;; 与 display-width 的一致性：前缀宽度 + 剩余宽度 = 整体宽度
(check (display-width-prefix "Hi金鱼" 3) => (+ (display-width-prefix "Hi金鱼" 2) 2))
(check (display-width-prefix "你好" 1) => (+ 0 (display-width "你")))

;; 常见 Emoji 字符（每个 2 列）
(check (display-width-prefix "😊ab" 1) => 2)
(check (display-width-prefix "😊ab" 2) => 3)

;; ANSI 序列不占用显示列宽，也不消耗字符配额
(check (display-width-prefix "\x1b[31mred\x1b[0m" 3) => 3)
(check (display-width-prefix "\x1b[33m金鱼\x1b[0m" 2) => 4)

;; 非法 UTF-8 字节：容错重算（截断的"好"替换为一个 U+FFFD，宽 1）
(check (display-width-prefix (string-append "你" (byte-vector->string #u8(229 165))) 2) => 3)

;; 类型错误检查
(check-catch 'type-error (display-width-prefix 123 1))
(check-catch 'type-error (display-width-prefix "abc" -1))
(check-catch 'type-error (display-width-prefix "abc" 'a))

(check-report)
