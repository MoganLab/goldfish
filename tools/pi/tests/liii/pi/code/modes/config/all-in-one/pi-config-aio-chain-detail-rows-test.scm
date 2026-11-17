;; pi-config-aio-chain-detail-rows
;; 生成 all-in-one 链条右列详情行（含节点链路与全局参数）。
;;
;; 语法
;; ----
;; (pi-config-aio-chain-detail-rows aio chain)
;;
;; 参数
;; ----
;; aio : list
;; pi-config-aio-state 返回的 alist。
;; chain : list
;; 单条链的 alist。
;;
;; 返回值
;; ------
;; list
;; ((label value) ...) 详情行列表。

(import (scheme base)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi code modes config state)
  (liii pi code modes config all-in-one)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path-join (path-temp-dir)
    (string-append "pi-cfg-aio-cdetail-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir TMP-DIR :parents #t :exist-ok #t)

(let* ((st (pi-config-state (path->string TMP-DIR) :env '()))
       (aio (pi-config-aio-state st))
       (chain (car (pi-config-aio-chains aio)))
       (rows (pi-config-aio-chain-detail-rows aio chain))
       (node1 (assoc "node #1" rows))
      ) ;
  (check (cadr (assoc "chain" rows)) => "coding-fast (快速)")
  (check (cadr (assoc "nodes" rows)) => "3")
  (check (and node1 (string-contains? (cadr node1) "google/gemini-3.8-flash"))
    =>
    #t
  ) ;check
  (check (cadr (assoc "maxWait" rows)) => "3600s")
  (check (cadr (assoc "retry" rows)) => "60s")
  (check (cadr (assoc "probe" rows)) => "8s")
  (check (cadr (assoc "source" rows)) => "(builtin default)")
) ;let*

(rmtree (path->string TMP-DIR))

(check-report)
