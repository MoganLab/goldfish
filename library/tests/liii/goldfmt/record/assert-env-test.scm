;; assert-env
;; 断言一棵 env 树满足基本结构约束，不满足时抛出异常。
;;
;; 语法
;; ----
;; (assert-env env)
;;
;; 参数
;; ----
;; env : env?
;; 待检查的环境节点。
;;
;; 返回值
;; ----
;; boolean?
;; 返回 #t 表示 env 树满足约束。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; 测试合法结构返回 #t
(check (assert-env (make-env :tag-name "root" :depth 0 :indent 0)) => #t)

;; 测试非法结构抛出 value-error
(check-catch 'value-error
  (assert-env (make-env :tag-name "root" :depth 0 :indent 2))
) ;check-catch

(check-report)
