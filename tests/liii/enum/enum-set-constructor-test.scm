(import (liii check)
        (liii enum)
        (srfi srfi-1)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-set (enum-type->enum-set color))

(define reddish
  (list->enum-set color
                  (map
                    (lambda (name)
                      (enum-name->enum color name)
                    ) ;lambda
                    (take color-names 3)
                  ) ;map
  ) ;list->enum-set
) ;define

;; enum-set-constructor
;; 返回一个从名称列表构造 enum-set 的过程。
;;
;; 语法
;; ----
;; (enum-set-constructor enum-set)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 用于确定目标 enum-type 的集合。
;;
;; 返回值
;; ----
;; procedure?
;; 接受名称列表并返回 enum-set 的过程。
;;
;; 注意
;; ----
;; 构造出的集合类型与传入集合一致。
;;
;; 示例
;; ----
;; (enum-set=? ((enum-set-constructor reddish) color-names) color-set) => #t
;;
;; 错误处理
;; ----
;; 无。

(let ((color-con (enum-set-constructor reddish)))
  (check (eqv? (enum-set-type (color-con '(green))) color) => #t)
  (check (enum-set=? (color-con color-names) color-set) => #t)
) ;let

(check-report)
