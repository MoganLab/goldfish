(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; generator->string
;; 将字符生成器产出的所有或指定数量的字符收集为字符串。
;;
;; 语法
;; ----
;; (generator->string gen)
;; (generator->string gen n)
;;
;; 参数
;; ----
;; gen : procedure
;; 字符生成器过程（产出的元素应为字符）。
;;
;; n : exact-nonnegative-integer (可选)
;; 最大收集字符数量。
;;
;; 返回值
;; ----
;; string
;; 收集得到的字符串。
;;
;; 错误处理
;; ----
;; 若生成器产出的元素不是字符，可能引发错误。

(let ((g (generator #\a #\b)))
  (check (generator->string g) => "ab")
)

(let ((g (generator #\a #\b #\c #\d)))
  (check (generator->string g 2) => "ab")
)

(let ((g (generator)))
  (check (generator->string g) => "")
)

(check-report)
