(import (liii check)
        (rename (srfi srfi-78)
                (check-reset! srfi-78-check-reset!)
        ) ;rename
) ;import

(check-set-mode! 'summary)

;; check-failed?
;; 判断当前测试上下文中是否已经存在失败断言。
;;
;; 语法
;; ----
;; (check-failed?)
;;
;; 使用场景
;; ----
;; 1. 在自定义测试辅助代码里探测是否已有失败。
;; 2. 在复杂测试流程中决定是否继续后续检查。
;; 3. 与底层重置函数配合，验证测试状态是否被清空。

(check-false (check-failed?))

(check 1 => 2)

(check-true (check-failed?))

(srfi-78-check-reset!)

(check-false (check-failed?))

(check-report)
