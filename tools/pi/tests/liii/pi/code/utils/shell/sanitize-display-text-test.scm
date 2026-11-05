;; sanitize-display-text
;; 清理来自模型、用户、工具或文件内容的不可信显示文本。
;;
;; 语法
;; ----
;; (sanitize-display-text str)
;;
;; 参数
;; ----
;; str : string?
;; 不可信显示文本。
;;
;; 返回值
;; ----
;; string
;; 移除了 ANSI 转义序列、二进制控制字符且修复了非法 UTF-8 的安全显示文本。
;;
;; 注意
;; ----
;; - 先调用 strip-ansi 剥除所有 ANSI 转义序列（颜色 SGR、光标控制 CSI、OSC 序列等）；
;; - 再调用 sanitize-binary-output 清理不可打印控制字符及特殊不可见注释；
;; - 保留 \t、\n、\r；
;; - 非法 UTF-8 字节序列替换为 U+FFFD。
;;
;; 错误处理
;; ----
;; type-error 当 str 不是字符串时。

(import (liii check)
        (liii string)
        (liii pi code utils shell))

(check-set-mode! 'report-failed)

;; 空字符串
(check (sanitize-display-text "") => "")

;; 普通文本
(check (sanitize-display-text "hello world") => "hello world")
(check (sanitize-display-text "你好，世界") => "你好，世界")

;; 剥除 ANSI SGR 颜色序列
(check (sanitize-display-text "\x1b;[31mred\x1b;[0m") => "red")
(check (sanitize-display-text "\x1b;[1;32mbold green\x1b;[0m") => "bold green")

;; 剥除 ANSI CSI 光标控制序列
(check (sanitize-display-text "hello\x1b;[2Kworld") => "helloworld")

;; 剥除 ANSI OSC 序列（如设置窗口标题）
(check (sanitize-display-text "\x1b;]0;title\x07;text") => "text")

;; 删除 C0 控制字符（保留 \t \n \r）
(check (sanitize-display-text "a\x00;b\x01;c") => "abc")
(check (sanitize-display-text "line1\nline2\tline3\rline4") => "line1\nline2\tline3\rline4")

;; 删除 U+FFF9..U+FFFB
(check (sanitize-display-text (string-append "a" (utf8->string #u8(239 191 185)) "b")) => "ab")

;; 修复非法 UTF-8
(check (sanitize-display-text (string-append "a" (byte-vector->string #u8(255)) "b"))
  =>
  (string-append "a" (utf8->string #u8(239 191 189)) "b")
) ;check

;; 组合测试：ANSI 颜色 + 控制字符 + 非法 UTF-8
(check (sanitize-display-text (string-append "\x1b;[31m" "hi\x00;" (byte-vector->string #u8(255)) "\x1b;[0m"))
  =>
  (string-append "hi" (utf8->string #u8(239 191 189)))
) ;check

;; 类型错误检查
(check-catch 'type-error (sanitize-display-text 123))
(check-catch 'type-error (sanitize-display-text 'sym))
(check-catch 'type-error (sanitize-display-text #f))

(check-report)
