(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; alist-cons 函数测试
;;
;; 语法
;; ----
;; (alist-cons key datum alist)
;;
;; 参数
;; ----
;; key : any
;; 关联列表的键。
;;
;; datum : any
;; 关联列表的值。
;;
;; alist : list?
;; 现有的关联列表。
;;
;; 返回值
;; ------
;; list
;; 返回一个新的关联列表，将(key . datum)添加到alist的开头。
;;
;; 示例
;; ----
;; (alist-cons 'a 1 '()) => '((a . 1))
;; (alist-cons 'a 1 '((b . 2))) => '((a . 1) (b . 2))

(check (alist-cons 'a 1 '()) => '((a . 1)))
(check (alist-cons 'a 1 '((b . 2))) => '((a . 1) (b . 2)))

(check-report)
