;; %repl-word-at
;; 取出光标左侧的当前词。
;;
;; 语法
;; ----
;; (%repl-word-at text col)
;;
;; 参数
;; ----
;; text : string
;; 编辑器中的当前文本。
;; col : integer
;; 光标所在的列号。
;;
;; 返回值
;; ----
;; string
;; 光标左侧的当前词；如果光标左侧不是符号字符，则返回空字符串。

(import (scheme base) (liii check) (liii goldrepl complete))

(check-set-mode! 'report-failed)

;; 普通词
(check (%repl-word-at "string-ap" 9) => "string-ap")
(check (%repl-word-at "hello world" 5) => "hello")
(check (%repl-word-at "hello world" 11) => "world")

;; 词前面有其它符号时只取符号部分
(check (%repl-word-at "(string-ap" 10) => "string-ap")
(check (%repl-word-at "(map string-" 13) => "string-")

;; 光标左侧不是符号字符时返回空字符串
(check (%repl-word-at "(+ 1 2)" 0) => "")
(check (%repl-word-at "(+ 1 2)" 4) => "")
(check (%repl-word-at "" 0) => "")

;; 光标位置超出文本长度时按文本末尾处理
(check (%repl-word-at "abc" 100) => "abc")

;; 光标在词中间时只取光标左侧部分
(check (%repl-word-at "string-ap" 6) => "string")

(check-report)
