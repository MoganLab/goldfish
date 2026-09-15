(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; string-accumulator
;; 创建按输入顺序将字符收集为字符串的累加器。
;;
;; 语法
;; ----
;; (string-accumulator)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; procedure
;; 一个单参累加器过程。传入字符时存入内部缓存；传入 eof-object 时按接收顺序返回所收集字符构成的字符串。
;;
;; 错误处理
;; ----
;; 若传入非字符元素，在 finalize 时可能引发错误。

(let ((a (string-accumulator)))
  (a #\a)
  (a #\b)
  (check (a (eof-object)) => "ab")
)

(let ((a (string-accumulator)))
  (check (a (eof-object)) => "")
)

(check-report)
