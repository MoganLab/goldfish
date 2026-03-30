(import (liii check)
        (liii enum)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-red (enum-name->enum color 'red))

(define color-set (enum-type->enum-set color))

;; enum-set-remove!
;; 线性更新地移除满足谓词的成员。
;;
;; 语法
;; ----
;; (enum-set-remove! pred enum-set)
;;
;; 参数
;; ----
;; pred : procedure?
;; 移除谓词。
;;
;; enum-set : enum-set?
;; 待更新的集合。
;;
;; 返回值
;; ----
;; enum-set
;; 移除成员后的集合。
;;
;; 注意
;; ----
;; 会直接更新传入集合。
;;
;; 示例
;; ----
;; (enum-set-contains? (enum-set-remove! (lambda (e) (enum=? e color-red)) (enum-set-copy color-set)) color-red) => #f
;;
;; 错误处理
;; ----
;; 无。

(let ((removed (enum-set-remove! (lambda (e) (enum=? e color-red)) (enum-set-copy color-set))))
  (check (enum-set-contains? removed color-red) => #f)
) ;let

(check-report)
