;; pi-config-aio-node-detail-rows
;; 生成 all-in-one 节点右列详情行。
;;
;; 语法
;; ----
;; (pi-config-aio-node-detail-rows aio chain node)
;;
;; 参数
;; ----
;; aio : list
;; pi-config-aio-state 返回的 alist。
;; chain : list
;; 单条链的 alist。
;; node : list
;; 单个节点的 alist。
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
    (string-append "pi-cfg-aio-ndetail-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir TMP-DIR :parents #t :exist-ok #t)

(let* ((st (pi-config-state (path->string TMP-DIR) :env '(("GEMINI_API_KEY"
                                                           . "k"))))
       (aio (pi-config-aio-state st))
       (chain (car (pi-config-aio-chains aio)))
       (node (car (pi-config-aio-chain-nodes chain)))
       (rows (pi-config-aio-node-detail-rows aio chain node))
      ) ;
  (check (cadr (assoc "chain" rows)) => "coding-fast")
  (check (cadr (assoc "order" rows)) => "#1")
  (check (string-contains? (cadr (assoc "provider" rows)) "google") => #t)
  (check (cadr (assoc "model" rows)) => "gemini-3.8-flash")
  (check (cadr (assoc "thinking" rows)) => "medium")
  (check (cadr (assoc "key" rows)) => "$GEMINI_API_KEY")
) ;let*

(rmtree (path->string TMP-DIR))

(check-report)
