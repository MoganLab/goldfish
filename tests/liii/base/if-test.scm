(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; if
;; 测试 if 条件分支表达式。
;;
;; 语法
;; ----
;; (if test consequent alternate)
;;
;; 参数
;; ----
;; test : any?
;; consequent, alternate : expression
;;
;; 返回值
;; ----
;; any?
;; 根据 test 的真假返回对应分支结果。
;;
;; 注意
;; ----
;; 除 #f 外其余值都被视为真值。
;;
;; 示例
;; ----
;; (if (> 3 2) 3 2) => 3
;;
;; 错误处理
;; ----
;; 按分支中表达式自身规则处理

(check (if (> 3 2) ((lambda () 3)) ((lambda () 2))) => 3)
(check (if (< 3 2) ((lambda () 3)) ((lambda () 2))) => 2)
(check (if (> 3 2) 3 2) => 3)
(check (if (< 3 2) 3 2) => 2)

(check (if (and (> 3 1) (< 3 4)) 'true-branch 'false-branch) => 'true-branch)
(check (if (or (> 3 4) (< 3 1)) 'true-branch 'false-branch) => 'false-branch)

(check-report)
