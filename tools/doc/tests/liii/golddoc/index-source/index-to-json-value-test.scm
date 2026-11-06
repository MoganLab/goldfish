;; index->json-value
;; 把函数索引转换为可序列化为 JSON 的值。
;;
;; 语法
;; ----
;; (index->json-value index)
;;
;; 参数
;; ----
;; index : list
;;   形如 `((函数名 . ("(库名)" ...)) ...)` 的函数索引。
;;
;; 返回值
;; ----
;; list?
;; 形如 `((函数名 . #("(库名)" ...)) ...)` 的值，库名集合以 vector 承载。
;;
;; 描述
;; ----
;; 索引中的库名列表转换为 vector，便于 `json->string` 直接序列化；
;; 函数名与库名的顺序保持不变。

(set! *load-path* (cons "tools/doc" *load-path*))

(import (liii check) (liii golddoc index-source))

(check-set-mode! 'report-failed)

(check (index->json-value '())
  =>
  '()
) ;check

(check (index->json-value (list (cons "alpha-run" (list "(liii alpha)"))))
  =>
  (list (cons "alpha-run" (vector "(liii alpha)")))
) ;check

(check (index->json-value (list (cons "shared-value" (list "(liii alpha)" "(liii beta)"))))
  =>
  (list (cons "shared-value" (vector "(liii alpha)" "(liii beta)")))
) ;check

(check-report)
