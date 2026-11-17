;; pi-config-aio-chain-nodes
;; 获取链条中的有序节点列表。
;;
;; 语法
;; ----
;; (pi-config-aio-chain-nodes chain)
;;
;; 参数
;; ----
;; chain : list
;; 单条链的 alist。
;;
;; 返回值
;; ------
;; list
;; 节点 alist 列表。

(import (scheme base)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii pi code modes config state)
  (liii pi code modes config all-in-one)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path-join (path-temp-dir)
    (string-append "pi-cfg-aio-nodes-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir TMP-DIR :parents #t :exist-ok #t)

(let* ((st (pi-config-state (path->string TMP-DIR) :env '()))
       (aio (pi-config-aio-state st))
       (chains (pi-config-aio-chains aio))
       (nodes (pi-config-aio-chain-nodes (car chains)))
      ) ;
  (check (length nodes) => 3)
  (check (cdr (assoc "index" (car nodes))) => 1)
  (check (cdr (assoc "provider" (car nodes))) => "google")
  (check (cdr (assoc "index" (cadr nodes))) => 2)
) ;let*

(check (pi-config-aio-chain-nodes '()) => '())

(rmtree (path->string TMP-DIR))

(check-report)
