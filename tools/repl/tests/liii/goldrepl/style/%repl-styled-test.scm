;; %repl-styled
;; 用指定样式包裹文本，并追加复位控制序列。
;;
;; 语法
;; ----
;; (%repl-styled name text)
;;
;; 参数
;; ----
;; name : string
;; 样式名，例如 "error"、"prompt"。
;; text : string
;; 需要着色的文本。
;;
;; 返回值
;; ----
;; string
;; 带 ANSI 控制序列的文本；样式名不存在时原样返回文本。

(import (scheme base) (liii check) (liii string) (liii goldrepl style))

(check-set-mode! 'report-failed)

;; 按样式着色并追加复位序列
(check (%repl-styled "error" "oops") => "\x1b;[31moops\x1b;[0m")
(check (%repl-styled "prompt" "gf> ") => "\x1b;[33mgf> \x1b;[0m")

;; 未定义的样式原样返回文本
(check (%repl-styled "no-such-style" "x") => "x")

(check-report)
