;; fix-line-tokens
;; 读取行记录中的 token 列表。
;;
;; 语法
;; ----
;; (fix-line-tokens line)
;;
;; 参数
;; ----
;; line : fix-line? 行记录
;;
;; 返回值
;; ------
;; list? 该行的 token 列表
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的 token 列表
(let* ((first (make-fix-token :text "a"))
       (second (make-fix-token :text "b"))
       (line (make-fix-line :tokens (list first second)))
      ) ;
  (check (fix-line-tokens line) => (list first second))
) ;let*

;; 测试：缺省为空列表
(check (fix-line-tokens (make-fix-line)) => '())

(check-report)
