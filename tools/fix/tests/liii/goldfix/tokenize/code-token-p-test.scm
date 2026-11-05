;; code-token?
;; 判断 token 是否为 code token：必须是 fix-token，且不是行注释或块注释。
;;
;; 语法
;; ----
;; (code-token? token)
;;
;; 参数
;; ----
;; token : any? 待判断的 token
;;
;; 返回值
;; ------
;; boolean? 是 code token 时为 #t，否则为 #f

(import (liii check) (liii goldfix tokenize) (liii goldfix record))

(check-set-mode! 'report-failed)

;; 测试：普通记号是 code token
(check (code-token? (car (tokenize "a"))) => #t)

;; 测试：左括号是 code token
(check (code-token? (car (tokenize "("))) => #t)

;; 测试：行注释不是 code token
(check (code-token? (car (tokenize ";x"))) => #f)

;; 测试：块注释不是 code token
(check (code-token? (car (tokenize "#|x|#"))) => #f)

;; 测试：非 fix-token 返回 #f
(check (code-token? 'a) => #f)
(check (code-token? #f) => #f)

(check-report)
