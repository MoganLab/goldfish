;; pi-config-aio-delete-chain
;; 生成从 all-in-one.json 删除指定伪模型（chain）的写入计划。
;;
;; 语法
;; ----
;; (pi-config-aio-delete-chain st chain-id)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;; chain-id : string
;; 待删除链条 id。
;;
;; 返回值
;; ------
;; list
;; 包含 ok、message、writes 的 alist。

(import (scheme base)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi code modes config state)
  (liii pi code modes config aio-writes)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path-join (path-temp-dir)
    (string-append "pi-cfg-aio-del-chain-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir TMP-DIR :parents #t :exist-ok #t)

(let ((st (pi-config-state (path->string TMP-DIR) :env '())))
  (let* ((r (pi-config-aio-delete-chain st "coding-fast"))
         (writes (cdr (assoc "writes" r)))
        ) ;
    (check (cdr (assoc "ok" r)) => #t)
    (check (string-contains? (cdar writes) "\"coding-fast\"") => #f)
    (check (string-contains? (cdar writes) "\"coding-high-quality\"") => #t)
  ) ;let*
  (check (cdr (assoc "ok" (pi-config-aio-delete-chain st "missing"))) => #f)
) ;let

(rmtree (path->string TMP-DIR))

(check-report)