(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; generate-temporaries
;; 为输入列表中的每个元素生成一个新的、互不冲突的唯一临时标识符。
;;
;; 语法
;; ----
;; (generate-temporaries l)
;;
;; 参数
;; ----
;; l : list
;; 任意列表（通常是宏参数列表或变量列表），其长度决定生成的临时变量个数。
;;
;; 返回值
;; -----
;; list of identifier
;; 与 l 长度相同的新临时标识符列表。

;; 1. 生成指定长度的临时变量列表
(let ((temps (generate-temporaries '(a b c))))
  (check (length temps) => 3)
  (check-true (identifier? (car temps)))
  (check-false (eq? (car temps) (cadr temps)))
  (check-false (eq? (cadr temps) (caddr temps))))

;; 2. 空列表生成空结果
(check (generate-temporaries '()) => '())

;; 3. 单个元素列表
(let ((temps (generate-temporaries '(x))))
  (check (length temps) => 1)
  (check-true (identifier? (car temps))))

(check-report)
