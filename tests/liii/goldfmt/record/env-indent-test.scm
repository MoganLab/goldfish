;; env-indent
;; 获取 env 节点的缩进量。
;;
;; 语法
;; ----
;; (env-indent env)
;;
;; 参数
;; ----
;; env : env?
;; 目标环境节点。
;;
;; 返回值
;; ----
;; integer?
;; 节点的缩进量，-1 表示缩进尚未确认。

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
  (check (env-indent e) => 2)
) ;let

(check-report)
