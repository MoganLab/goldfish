;; %repl-highlight-line
;; 对一行 Scheme 文本做语法高亮，返回带 ANSI 控制序列的文本。
;;
;; 语法
;; ----
;; (%repl-highlight-line line)
;;
;; 参数
;; ----
;; line : string
;; 需要高亮的单行文本。
;;
;; 返回值
;; ----
;; string
;; 带 ANSI 控制序列的高亮文本。

(import (scheme base) (liii check) (liii string) (liii goldrepl style))

(check-set-mode! 'report-failed)

;; 普通代码与注释
(check-true (string? (%repl-highlight-line "(define (f x) (+ x 1))")))
(check-true (string? (%repl-highlight-line "; comment text")))

;; 注释使用灰色样式
(check-true (string-contains (%repl-highlight-line "; comment") "\x1b;[90m"))

;; 关键字使用关键字样式
(check-true (string-contains (%repl-highlight-line "(define x 100)")
              "\x1b;[35mdefine\x1b;[0m"
            ) ;string-contains
) ;check-true

;; 数字使用数字样式
(check-true (string-contains (%repl-highlight-line "(define x 100)") "\x1b;[33m100\x1b;[0m")
) ;check-true

;; 中文等多字节字符不会被破坏
(let ((hl (%repl-highlight-line "(define 你好 100)")))
  (check-true (string-contains hl "\x1b;[35mdefine\x1b;[0m"))
  (check-true (string-contains hl "你好"))
) ;let

(let ((hl (%repl-highlight-line "(display \"你好世界\")")))
  (check-true (string-contains hl "你好世界"))
) ;let

;; 布尔常量使用常量样式
(check-true (string-contains (%repl-highlight-line "(if #t 1 2)") "\x1b;[34m#t\x1b;[0m")
) ;check-true

;; 转义引号
(let ((hl (%repl-highlight-line "(display \"a\\\"b\")")))
  (check-true (string-contains hl "a\\\"b"))
) ;let

;; 未闭合字符串不会崩溃，剩余部分整体使用字符串样式
(let ((hl (%repl-highlight-line "(display \"abc")))
  (check-true (string-contains hl "\x1b;[32m\"abc\x1b;[0m"))
) ;let

(check-report)
