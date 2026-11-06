;; env-left-line
;; 获取 env 节点左侧标记所在的行号。
;;
;; 语法
;; ----
;; (env-left-line env)
;;
;; 参数
;; ----
;; env : env?
;; 目标环境节点。
;;
;; 返回值
;; ----
;; integer?
;; 左侧标记所在行号，0 表示行号未确定。

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
  (check (env-left-line e) => 3)
) ;let

(check-report)
