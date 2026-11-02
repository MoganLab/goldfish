;; display-rule-result
;; 输出单条规则的检查结果行到当前输出端口。
;;
;; 语法
;; ----
;; (display-rule-result result)
;;
;; 参数
;; ----
;; result : list  规则检查结果三元组 (name ok elapsed)
;;
;; 返回值
;; ------
;; unspecified
;; 将结果行及换行符打印到当前输出端口。

(import (scheme base)
  (scheme write)
  (liii check)
  (liii string)
  (liii goldcheck report)
) ;import

(check-set-mode! 'report-failed)

(let* ((out
         (with-output-to-string (lambda () (display-rule-result (list "my-rule" #t 0.042)))
         ) ;with-output-to-string
       ) ;out
      ) ;
  (check-true (string-contains? out "[OK    ]"))
  (check-true (string-contains? out "my-rule"))
  (check-true (string-contains? out "0.042s"))
) ;let*

(check-report)
