;; pi-config-aio-set-slot-thinking
;; 生成修改指定 all-in-one 槽位思考强度的写入计划。
;;
;; 语法
;; ----
;; (pi-config-aio-set-slot-thinking st chain-id node-index level)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;; chain-id : string
;; 链条 id。
;; node-index : integer
;; 0-based 节点下标。
;; level : string
;; 思考等级。
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
    (string-append "pi-cfg-aio-set-tl-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir TMP-DIR :parents #t :exist-ok #t)

(let ((st (pi-config-state (path->string TMP-DIR) :env '())))
  (let* ((r (pi-config-aio-set-slot-thinking st "coding-fast" 0 "high"))
         (writes (cdr (assoc "writes" r)))
        ) ;
    (check (cdr (assoc "ok" r)) => #t)
    (check (string-contains? (cdar writes) "\"thinkingLevel\": \"high\"") => #t)
  ) ;let*
  (check (cdr (assoc "ok" (pi-config-aio-set-slot-thinking st "coding-fast" 99 "high"))) => #f)
) ;let

(rmtree (path->string TMP-DIR))

(check-report)