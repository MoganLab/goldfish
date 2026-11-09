;; %syntax-case-close-identifier
;; 将标识符在指定环境中闭包化为句法闭包。
;;
;; 语法
;; ----
;; (%syntax-case-close-identifier id env)
;;
;; 参数
;; ----
;; id : identifier
;; 待闭包化的标识符或句法闭包。
;;
;; env : environment
;; 词法环境。
;;
;; 返回值
;; -----
;; syntax / identifier
;; 闭包化后的标识符。

(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; 1. 闭包化标识符
(let ((closed (%syntax-case-close-identifier 'car (rootlet))))
  (check-true (or (symbol? closed) (identifier? closed))))

(check-report)
