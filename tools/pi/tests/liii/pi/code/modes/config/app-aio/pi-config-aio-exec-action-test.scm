;; pi-config-aio-exec-action
;; 执行 all-in-one 子标签页的交互动作并返回结果。
;;
;; 语法
;; ----
;; (pi-config-aio-exec-action st cursor act)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;; cursor : list
;; 当前光标。
;; act : (or symbol pair)
;; 动作描述符。
;;
;; 返回值
;; ------
;; list
;; (next-modal message reload?)。

(import (scheme base)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi code modes config state)
  (liii pi code modes config app-aio)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path-join (path-temp-dir)
    (string-append "pi-cfg-aio-exec-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir TMP-DIR :parents #t :exist-ok #t)

(let ((st (pi-config-state (path->string TMP-DIR) :env '())))
  (let ((r1 (pi-config-aio-exec-action st '() 'new-chain)))
    (check (cdr (assoc 'mode (car r1))) => 'new-chain)
    (check (caddr r1) => #f)
  ) ;let
  (let ((r2 (pi-config-aio-exec-action st '() '(delete-chain . "coding-fast"))))
    (check (car r2) => #f)
    (check (caddr r2) => #t)
    (check (string-contains? (cadr r2) "deleted chain coding-fast") => #t)
  ) ;let
) ;let

(rmtree (path->string TMP-DIR))

(check-report)