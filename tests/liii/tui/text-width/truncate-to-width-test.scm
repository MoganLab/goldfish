(import (liii check)
        (liii tui text-width))

(check-set-mode! 'report-failed)

;; truncate-to-width
;; 按终端视觉列宽截断单行文本，可选自定义省略符号，且保证 ANSI 序列与双宽字符安全。
;;
;; 语法
;; ----
;; (truncate-to-width str width [ellipsis])
;;
;; 参数
;; ----
;; str : string
;; 待截断的单行文本。
;; width : real
;; 目标视觉显示宽度（列数）。若 <= 0 则返回空字符串。
;; ellipsis : string (可选，默认 "...")
;; 超出宽度时附加的省略指示串。
;;
;; 返回值
;; ----
;; string
;; 截断后的安全字符串，其 display-width <= width。
;;
;; 描述
;; ----
;; - 若 str 的 display-width <= width，原样返回 str；
;; - 若截断发生，保留前半部分字符并在尾部附加 ellipsis；
;; - 不劈裂 CJK 双宽字符：若剩余可用宽度为 1 且下个字符为双宽字符，则提前截断；
;; - ANSI 安全闭合：若原文本包含 ANSI 转义样式且发生截断，尾部自动补充 \x1b[0m；
;; - 极小宽度处理：当 width 小于省略号宽度时，安全截取省略号本身。
;;
;; 错误处理
;; ----
;; type-error 当 str 或 ellipsis 不是字符串，或 width 不是数字时。

;; 空字符串与零宽度/负宽度
(check (truncate-to-width "" 10) => "")
(check (truncate-to-width "hello" 0) => "")
(check (truncate-to-width "hello" -5) => "")

;; 正好等宽或更短：原样返回
(check (truncate-to-width "hello" 5) => "hello")
(check (truncate-to-width "hello" 10) => "hello")
(check (truncate-to-width "金鱼" 4) => "金鱼")

;; ASCII 截断
(check (truncate-to-width "hello world" 8) => "hello...")
(check (truncate-to-width "hello world" 5) => "he...")

;; 自定义省略号
(check (truncate-to-width "hello world" 8 "!") => "hello w!")
(check (truncate-to-width "hello world" 5 "") => "hello")

;; CJK 双宽字符截断（不劈裂中文字符）
;; "你好世界"：每个汉字 2 列，共 8 列。截断到 7 列，省略号占 3 列，可用 4 列 -> "你好" 占 4 列 -> "你好..." 占 7 列
(check (truncate-to-width "你好世界" 7) => "你好...")

;; 若可用宽度仅剩 1 列，无法放下下一个 2 列汉字，应提前截断：
;; 可用宽度 = 6 - 3 = 3 列；第一个字 "你" 占 2 列，剩余 1 列放不下 "好"，因此只能放 "你"，最终 "你..."
(check (truncate-to-width "你好世界" 6) => "你...")

;; 极小宽度边界（width < ellipsis 宽度）
(check (truncate-to-width "hello world" 2 "...") => "..")
(check (truncate-to-width "hello world" 1 "...") => ".")

;; ANSI 转义序列安全截断与尾部闭合
;; 包含颜色的文本截断后必须在尾部复位 \x1b[0m 防止颜色泄漏
(check (truncate-to-width "\x1b[31mhello world\x1b[0m" 8) => "\x1b[31mhello...\x1b[0m")
(check (truncate-to-width "\x1b[32m你好世界\x1b[0m" 7) => "\x1b[32m你好...\x1b[0m")

;; 类型错误检查
(check-catch 'type-error (truncate-to-width 123 5))
(check-catch 'type-error (truncate-to-width "hello" "5"))
(check-catch 'type-error (truncate-to-width "hello" 5 123))

(check-report)
