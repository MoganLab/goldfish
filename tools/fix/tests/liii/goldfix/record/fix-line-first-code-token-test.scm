;; fix-line-first-code-token
;; 读取行记录中第一枚 code token。
;;
;; 语法
;; ----
;; (fix-line-first-code-token line)
;;
;; 参数
;; ----
;; line : fix-line? 行记录
;;
;; 返回值
;; ------
;; fix-token? 第一枚 code token，没有则为 #f
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的第一枚 code token
(let* ((token (make-fix-token :type 'other :text "define"))
       (line (make-fix-line :first-code-token token))
      ) ;
  (check (fix-line-first-code-token line) => token)
) ;let*

;; 测试：缺省为 #f
(check (fix-line-first-code-token (make-fix-line)) => #f)

(check-report)
