;; append-line-lists
;; 依序拼接多组行列表，不插入空白行，空组跳过。
;;
;; 语法
;; ----
;; (append-line-lists line-lists)
;;
;; 参数
;; ----
;; line-lists : list
;; 行列表的列表。
;;
;; 返回值
;; ----
;; list
;; 拼接后的行列表。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown util))

(check-set-mode! 'report-failed)

(check-true (procedure? append-line-lists))

(check (append-line-lists '()) => '())
(check (append-line-lists '(("a"))) => '("a"))
(check (append-line-lists '(("a") ("b" "c"))) => '("a" "b" "c"))
(check (append-line-lists '(("a") () ("b"))) => '("a" "b"))

(check-report)
