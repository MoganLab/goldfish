(import (liii check) (liii tui text-width))

(check-set-mode! 'report-failed)

;; wrap-text
;; 按终端视觉显示宽度对文本进行安全折行，返回行字符串列表。
;;
;; 语法
;; ----
;; (wrap-text str width)
;;
;; 参数
;; ----
;; str : string?
;; 待折行文本，可包含换行符 \n。
;;
;; width : real?
;; 目标视觉显示宽度（列数）。若 <= 0 则返回 '（"")。
;;
;; 返回值
;; ----
;; (list string ...)
;; 折行后的行文本列表。
;;
;; 注意
;; ----
;; - 支持原文本中的 \n 分段；
;; - 英文词语优先在空格处断行（word wrap）；
;; - 遇到超长单词或无空格长文本（如 CJK 表意字符），按显示宽度硬折行；
;; - CJK 双宽字符绝不劈半跨行；
;; - ANSI 转义序列不计宽度，折行绝不切断转义码；
;; - 输入含非法 UTF-8 字节时先替换为 U+FFFD 再折行（合法输入返回原串对象）。
;;
;; 错误处理
;; ----
;; type-error 当 str 不是字符串或 width 不是数字时。

;; 空字符串与零/负宽度
(check (wrap-text "" 10) => '(""))
(check (wrap-text "hello" 0) => '(""))
(check (wrap-text "hello" -3) => '(""))

;; 短文本无需折行
(check (wrap-text "hello" 10) => '("hello"))
(check (wrap-text "金鱼" 4) => '("金鱼"))

;; 英文词按空格折行
(check (wrap-text "hello world foo" 11) => '("hello world" "foo"))
(check (wrap-text "one two three four" 9) => '("one two" "three" "four"))

;; 英文超长词硬折行
(check (wrap-text "abcdefghij" 4) => '("abcd" "efgh" "ij"))
(check (wrap-text "12,34,56" 5) => '("12,34" ",56"))

;; 原文本自带换行符
(check (wrap-text "hello\nworld" 10) => '("hello" "world"))
(check (wrap-text "a\n\nb" 10) => '("a" "" "b"))

;; CJK 文本按双宽字符折行
(check (wrap-text "你好世界" 4) => '("你好" "世界"))
;; 奇数行宽边界：5 列宽放不下第三个汉字（2+2=4 剩 1 列放不下 2 列的“世”），故提前换行
(check (wrap-text "你好世界" 5) => '("你好" "世界"))

;; 中英文混合折行
(check (wrap-text "你好 world" 7) => '("你好" "world"))

;; 首行缩进与前导空格保留（不误删代码或预格式化缩进）
(check (wrap-text "  hello world" 8) => '("  hello" "world"))
(check (wrap-text "line1\n  indented" 15) => '("line1" "  indented"))

;; ANSI 序列安全折行（ANSI 码不占显示宽度，且不被切碎）
(check (wrap-text "\x1b;[31mhello world\x1b;[0m" 5)
  =>
  '("\x1b;[31mhello\x1b;[0m" "\x1b;[31mworld\x1b;[0m")
) ;check

;; 裸 ESC（非 CSI 开头，如 OSC 的 ESC]）不导致死循环，计 0 宽丢弃
(check (wrap-text "a\x1b;]0;tb" 80) => '("a]0;tb"))
(check (wrap-text "x\x1b;" 80) => '("x"))

;; 非法 UTF-8 字节：入口替换为 U+FFFD 后再折行，不抛 value-error
(check (wrap-text (string-append "好" (byte-vector->string #u8(229 165)) "ab") 5)
  =>
  (list (string-append "好" (utf8->string #u8(239 191 189)) "ab"))
) ;check

;; 类型错误检查
(check-catch 'type-error (wrap-text 123 10))
(check-catch 'type-error (wrap-text "hello" "10"))
(check-catch 'type-error (wrap-text #f 10))

(check-report)
