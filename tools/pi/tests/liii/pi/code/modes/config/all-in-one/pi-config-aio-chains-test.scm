;; pi-config-aio-chains
;; 获取 all-in-one 状态中的链条列表。
;;
;; 语法
;; ----
;; (pi-config-aio-chains aio)
;;
;; 参数
;; ----
;; aio : list
;; pi-config-aio-state 返回的 alist。
;;
;; 返回值
;; ------
;; list
;; 链条 alist 列表。

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
    (string-append "pi-cfg-aio-chains-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir TMP-DIR :parents #t :exist-ok #t)

(let* ((st (pi-config-state (path->string TMP-DIR) :env '()))
       (aio (pi-config-aio-state st))
       (chains (pi-config-aio-chains aio))
      ) ;
  (check (length chains) => 2)
  (check (cdr (assoc "id" (car chains))) => "coding-fast")
  (check (cdr (assoc "id" (cadr chains))) => "coding-high-quality")
) ;let*

(check (pi-config-aio-chains '()) => '())

(rmtree (path->string TMP-DIR))

(check-report)
