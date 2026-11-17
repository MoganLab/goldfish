;; pi-config-aio-delete-slot
;; 生成从指定 all-in-one 链条删除指定下标槽位的写入计划。
;;
;; 语法
;; ----
;; (pi-config-aio-delete-slot st chain-id node-index)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;; chain-id : string
;; 链条 id。
;; node-index : integer
;; 0-based 节点下标。
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
    (string-append "pi-cfg-aio-del-slot-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir TMP-DIR :parents #t :exist-ok #t)

(let ((st (pi-config-state (path->string TMP-DIR) :env '())))
  (let* ((r (pi-config-aio-delete-slot st "coding-fast" 0))
         (writes (cdr (assoc "writes" r)))
        ) ;
    (check (cdr (assoc "ok" r)) => #t)
    (check (string-contains? (cdar writes) "gemini-3.8-flash") => #f)
    (check (string-contains? (cdar writes) "MiniMax-M3.1-Flash") => #t)
  ) ;let*
  (check (cdr (assoc "ok" (pi-config-aio-delete-slot st "coding-fast" 99))) => #f)
) ;let

(rmtree (path->string TMP-DIR))

(check-report)