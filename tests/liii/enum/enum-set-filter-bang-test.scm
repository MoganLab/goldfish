(import (liii check) (liii enum))


(check-set-mode! 'report-failed)


(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define


(define color
  (make-enum-type color-names)
) ;define


(define color-red
  (enum-name->enum color 'red)
) ;define


(define color-set
  (enum-type->enum-set color)
) ;define


;; enum-set-filter!
;; 线性更新地保留满足谓词的成员。
;;
;; 语法
;; ----
;; (enum-set-filter! pred enum-set)
;;
;; 参数
;; ----
;; pred : procedure?
;; 过滤谓词。
;;
;; enum-set : enum-set?
;; 待更新的集合。
;;
;; 返回值
;; ----
;; enum-set
;; 过滤后的集合。
;;
;; 注意
;; ----
;; 会直接更新传入集合。
;;
;; 示例
;; ----
;; (enum-set-size (enum-set-filter! (lambda (e) (enum=? e color-red)) (enum-set-copy color-set))) => 1
;;
;; 错误处理
;; ----
;; 无。


(let ((filtered (enum-set-filter! (lambda (e) (enum=? e color-red))
                  (enum-set-copy color-set)
                ) ;enum-set-filter!
      ) ;filtered
     ) ;
  (check (enum-set-size filtered) => 1)
  (check (enum-set-contains? filtered color-red)
    =>
    #t
  ) ;check
) ;let


(check-report)
