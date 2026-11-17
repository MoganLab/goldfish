;; pi-config-aio-status-text
;; 组装 all-in-one 子标签页底部的状态栏文本。
;;
;; 语法
;; ----
;; (pi-config-aio-status-text st aio)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;; aio : list
;; pi-config-aio-state 返回的 alist。
;;
;; 返回值
;; ------
;; string
;; 状态栏文本。

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
    (string-append "pi-cfg-aio-status-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir TMP-DIR :parents #t :exist-ok #t)

(let* ((st (pi-config-state (path->string TMP-DIR) :env '()))
       (aio (pi-config-aio-state st))
       (txt (pi-config-aio-status-text st aio))
      ) ;
  (check (string-contains? txt "2 chains") => #t)
  (check (string-contains? txt "(builtin default)") => #t)
  (check (string-contains? txt "continuous on") => #t)
) ;let*

(rmtree (path->string TMP-DIR))

(check-report)
