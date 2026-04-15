(import (liii check) (liii enum))


(check-set-mode! 'report-failed)


(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define


(define color
  (make-enum-type color-names)
) ;define


(define color-set
  (enum-type->enum-set color)
) ;define


(define empty-colors
  (enum-empty-set color)
) ;define


(define (constantly obj)
  (lambda _ obj)
) ;define


(define never (constantly #f))


;; enum-set-any?
;; 判断 enum-set 中是否存在满足谓词的成员。
;;
;; 语法
;; ----
;; (enum-set-any? pred enum-set)
;;
;; 参数
;; ----
;; pred : procedure?
;; 判定每个 enum 的谓词。
;;
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; boolean
;; 存在满足谓词的成员时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 空集合总是返回 #f。
;;
;; 示例
;; ----
;; (enum-set-any? never empty-colors) => #f
;;
;; 错误处理
;; ----
;; 无。


(check (enum-set-any? (lambda (e) (eq? 'green (enum-name e)))
         color-set
       ) ;enum-set-any?
  =>
  #t
) ;check
(check (enum-set-any? (lambda (e) (eq? 'mauve (enum-name e)))
         color-set
       ) ;enum-set-any?
  =>
  #f
) ;check
(check (enum-set-any? never empty-colors)
  =>
  #f
) ;check


(check-report)
