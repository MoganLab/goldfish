(import (liii check) (scheme write))
(check-set-mode! 'report-failed)

;; write-string
;; 将字符串按 display 风格（不带引号）写入输出端口。
;;
;; 语法
;; ----
;; (write-string string)
;; (write-string string port)
;; (write-string string port start)
;; (write-string string port start end)
;;
;; 参数
;; ----
;; string : string?
;; 要输出的字符串。
;;
;; port : output-port? (可选)
;; 输出端口。省略时，写入当前输出端口。
;;
;; start : integer? (可选)
;; 起始下标（含），默认为 0。
;;
;; end : integer? (可选)
;; 结束下标（不含），默认为字符串长度。
;;
;; 返回值
;; ----
;; unspecified
;; 主要用于副作用输出。
;;
;; 描述
;; ----
;; 1. `write-string` 输出字符串内容本身，不带双引号（与 `display` 风格一致）。
;; 2. 可以通过 start 和 end 只输出字符串的一个区间。

(define (capture-output thunk)
  (let ((port (open-output-string)))
    (thunk port)
    (get-output-string port)
  ) ;let
) ;define

(check-true (procedure? write-string))

(check (capture-output (lambda (port) (write-string "goldfish" port))) => "goldfish")

;; 输出不带双引号，与 write 不同
(check (capture-output (lambda (port) (write-string "a\"b" port))) => "a\"b")

;; start 和 end 指定输出区间：[start, end)
(check (capture-output (lambda (port) (write-string "goldfish" port 4))) => "fish")
(check (capture-output (lambda (port) (write-string "goldfish" port 0 4))) => "gold")

(check-report)
