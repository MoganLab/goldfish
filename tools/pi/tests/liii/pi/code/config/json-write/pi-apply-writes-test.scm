;; pi-apply-writes
;; 按顺序原子写入多个文件，自动创建缺失的父目录。
;;
;; 语法
;; ----
;; (pi-apply-writes dir-str writes)
;;
;; 参数
;; ----
;; dir-str : string
;; 配置目录。
;; writes : list
;; (相对路径 . 文本) 列表。
;;
;; 返回值
;; ----
;; alist
;; 包含 ok、message 与 failed 的结果。

(import (liii check) (liii path) (liii shutil) (liii pi code config json-write))

(check-set-mode! 'report-failed)

(define DIR (path->string (path-join (path-temp-dir) "pi-apply-writes-test")))

(when (file-exists? DIR)
  (rmtree DIR #t))
(path-mkdir DIR :parents #t :exist-ok #t)

(define WRITES
  (list (cons "models/providers.json" "[\"google\"]\n")
    (cons "models/google/new.json" "{\"id\": \"new\"}\n")
  ) ;list
) ;define

(let ((res (pi-apply-writes DIR WRITES)))
  (check (cdr (assoc "ok" res)) => #t)
  (check (cdr (assoc "failed" res)) => #f)
) ;let

(check (path-read-text (path->string (path-join (path DIR) (path "models/providers.json"))))
  =>
  "[\"google\"]\n"
) ;check
(check (path-read-text (path->string (path-join (path DIR) (path "models/google/new.json"))))
  =>
  "{\"id\": \"new\"}\n"
) ;check

(rmtree DIR #t)

(check-report)
