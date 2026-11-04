;; env-tag-name
;; 获取 env 节点的标记名称。
;;
;; 语法
;; ----
;; (env-tag-name env)
;;
;; 参数
;; ----
;; env : env?
;; 目标环境节点。
;;
;; 返回值
;; ----
;; string?
;; 节点的标记名称，未命名时为空字符串。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

(let ((e (make-env
           :tag-name   "if"
           :depth      1
           :indent     2
           :left-line  3
           :right-line 4
           :value      '(if a b)
         ) ;make-env
      ) ;e
     ) ;
  (check (env-tag-name e) => "if")
) ;let

(check-report)
