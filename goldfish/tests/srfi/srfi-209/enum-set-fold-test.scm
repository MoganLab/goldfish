;; enum-set-fold
;; 对集合成员做折叠操作。
;;
;; 语法
;; ----
;; (enum-set-fold proc nil enum-set)
;;
;; 参数
;; ----
;; proc : procedure?
;; 折叠过程。
;;
;; nil : any?
;; 初始值。
;;
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; any?
;; 折叠后的结果。
;;
;; 注意
;; ----
;; 遍历顺序按序数排列。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii enum))


(check-set-mode! 'report-failed)


(define color-names '(red tangerine orange yellow green cyan blue violet))


(define color (make-enum-type color-names))


(define color-set (enum-type->enum-set color))

(check
  (enum-set-fold (lambda (enum lis) (cons (enum-name enum) lis)) '() color-set)
  =>
  (reverse color-names)
) ;check


(check-report)
