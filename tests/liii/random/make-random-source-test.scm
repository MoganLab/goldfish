(import (liii random)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; make-random-source
;; 创建一个新的随机源。
;;
;; 语法
;; ----
;; (make-random-source)
;;
;; 返回值
;; ----
;; random-source
;; 返回一个新的独立随机源对象。
;;
;; 示例
;; ----
;; (make-random-source)  => 新的随机源
;;
;; 注意
;; ----
;; 多个随机源是独立的，互不影响。

; 创建随机源
(let ((s1 (make-random-source))
      (s2 (make-random-source)))
  (check (random-source? s1) => #t)
  (check (random-source? s2) => #t)
) ;let

; 多个随机源独立
(let ((s1 (make-random-source))
      (s2 (make-random-source)))
  (let ((rand1 (random-source-make-integers s1))
        (rand2 (random-source-make-integers s2)))
    (let ((r1 (rand1 1000000))
          (r2 (rand2 1000000)))
      (check (integer? r1) => #t)
      (check (integer? r2) => #t)
    ) ;let
  ) ;let
) ;let

(check-report)
