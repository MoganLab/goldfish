;; env-value
;; 获取 env 节点保存的值。
;;
;; 语法
;; ----
;; (env-value env)
;;
;; 参数
;; ----
;; env : env?
;; 目标环境节点。
;;
;; 返回值
;; ----
;; any?
;; 节点保存的值，默认为 #f。

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
  (check (env-value e) => '(if a b))
) ;let

(check-report)
