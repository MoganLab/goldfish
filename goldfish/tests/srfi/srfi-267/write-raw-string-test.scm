;; write-raw-string
;; 把字符串按 raw string 字面量的形式写出。
;;
;; 语法
;; ----
;; (write-raw-string str delimiter [port])
;;
;; 参数
;; ----
;; str : string?
;; 待写出的字符串内容。
;; delimiter : string?
;; 用于包围内容的 delimiter，不能包含双引号。
;; port : output-port 可选
;; 输出目标端口，默认使用当前输出端口。
;; 返回值
;; ----
;; unspecified
;; 把 raw string 字面量写入端口。
;; 说明
;; ----
;; 写出的形式为 #"<delimiter>"<content>"<delimiter>"，
;; 可以用 read-raw-string 读回同样的内容。
;; 错误处理
;; ----
;; raw-string-write-error 当 delimiter 无法表示该字符串时抛出；
;; type-error 当 str 或 delimiter 不是字符串时抛出。

(import (liii check) (scheme file) (srfi srfi-267))


(check-set-mode! 'report-failed)


(let ((out (open-output-string)))
  (write-raw-string "hello" "" out)
  (check (get-output-string out) => "#\"\"hello\"\"")
) ;let

;; 写出的内容可以被读回
(let* ((payload "#\"\"a\"\" and \"quotes\"")
       (delimiter (generate-delimiter payload))
       (out (open-output-string))
      ) ;
  (write-raw-string payload delimiter out)
  (check (read-raw-string (open-input-string (get-output-string out))) => payload)
) ;let*

;; 错误处理
(check-catch 'raw-string-write-error
  (write-raw-string "\"" "" (open-output-string))
) ;check-catch

(check-catch 'type-error (write-raw-string 1 "" (open-output-string)))
(check-catch 'type-error (write-raw-string "abc" 1 (open-output-string)))

(check-report)
