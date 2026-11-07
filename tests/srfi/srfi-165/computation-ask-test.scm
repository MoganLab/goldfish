;; computation-ask
;; 创建请求当前计算环境的计算。
;;
;; 语法
;; ----
;; computation-ask
;;
;; 参数
;; ----
;; 无
;; 返回值
;; ----
;; computation
;; 返回以当前环境为结果的计算对象。
;; 说明
;; ----
;; 常与 computation-fn 配合使用，用于读取环境变量。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


(define v (make-computation-environment-variable 'x 42 #f))

;; 直接求值得到环境对象
(define env (computation-run (computation-ask)))
(check (computation-environment-ref env v) => 42)

;; 与 computation-bind 组合读取变量
(check
  (computation-run
    (computation-bind (computation-ask)
      (lambda (e) (computation-pure (computation-environment-ref e v)))
    )
  )
  =>
  42
) ;check

(check-report)
