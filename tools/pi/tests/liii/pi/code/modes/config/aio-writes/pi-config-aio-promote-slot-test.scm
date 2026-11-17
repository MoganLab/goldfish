;; pi-config-aio-promote-slot
;; 将指定 all-in-one 链条中的某个槽位置为链条第一个节点。
;;
;; 语法
;; ----
;; (pi-config-aio-promote-slot st chain-id node-index)
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
  (liii json)
  (liii os)
  (liii path)
  (liii shutil)
  (liii pi code modes config state)
  (liii pi code modes config aio-writes)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path-join (path-temp-dir)
    (string-append "pi-cfg-aio-promote-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir TMP-DIR :parents #t :exist-ok #t)

(let ((st (pi-config-state (path->string TMP-DIR) :env '())))
  ;; 1. 第 0 个已是首位
  (let ((r0 (pi-config-aio-promote-slot st "coding-fast" 0)))
    (check (cdr (assoc "ok" r0)) => #t)
    (check (cdr (assoc "writes" r0)) => '())
  ) ;let
  ;; 2. 把第 2 个槽位（index 1: minimax-cn）置顶到首位
  (let* ((r1 (pi-config-aio-promote-slot st "coding-fast" 1))
         (writes (cdr (assoc "writes" r1)))
         (parsed (string->json (cdar writes)))
         (c0 (vector-ref (cdr (assoc "chains" parsed)) 0))
         (nodes (cdr (assoc "nodes" c0)))
        ) ;
    (check (cdr (assoc "ok" r1)) => #t)
    (check (cdr (assoc "provider" (vector-ref nodes 0))) => "minimax-cn")
    (check (cdr (assoc "provider" (vector-ref nodes 1))) => "google")
    (check (cdr (assoc "provider" (vector-ref nodes 2))) => "deepseek")
  ) ;let*
  ;; 3. 越界拒绝
  (check (cdr (assoc "ok" (pi-config-aio-promote-slot st "coding-fast" 99))) => #f)
) ;let

(rmtree (path->string TMP-DIR))

(check-report)