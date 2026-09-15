(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gcons*
;; 在已有生成器产出的序列前面添加一个或多个前置元素。
;;
;; 语法
;; ----
;; (gcons* item ... gen)
;;
;; 参数
;; ----
;; item ... : any
;; 待前置插入的一个或多个元素。
;;
;; gen : procedure
;; 原始生成器。
;;
;; 返回值
;; ----
;; procedure
;; 新的生成器过程。先依次产出 item ...，随后产出 gen 的元素。
;;
;; 错误处理
;; ----
;; 无

(let ((g (gcons* 1 2 (generator 3 4))))
  (check (generator->list g) => '(1 2 3 4))
)

(let ((g (gcons* 'a (generator))))
  (check (generator->list g) => '(a))
)

(check-report)
