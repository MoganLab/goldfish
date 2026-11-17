;; pi-config-aio-file-view
;; 生成 all-in-one.json 的只读覆盖层文件视图。
;;
;; 语法
;; ----
;; (pi-config-aio-file-view st)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;;
;; 返回值
;; ------
;; list
;; 包含 kind、id、path、exists 与 content 的 alist。

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
    (string-append "pi-cfg-aio-fview-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir TMP-DIR :parents #t :exist-ok #t)

;; 1. 文件不存在
(let* ((st (pi-config-state (path->string TMP-DIR) :env '()))
       (view (pi-config-aio-file-view st))
      ) ;
  (check (cdr (assoc "kind" view)) => "all-in-one")
  (check (cdr (assoc "path" view)) => "all-in-one.json")
  (check (cdr (assoc "exists" view)) => #f)
) ;let*

;; 2. 文件存在
(path-write-text (path-join TMP-DIR "all-in-one.json")
  "{\"provider\":\"all-in-one\"}\n"
) ;path-write-text
(let* ((st (pi-config-state (path->string TMP-DIR) :env '()))
       (view (pi-config-aio-file-view st))
      ) ;
  (check (cdr (assoc "exists" view)) => #t)
  (check (string-contains? (cdr (assoc "content" view)) "all-in-one") => #t)
) ;let*

(rmtree (path->string TMP-DIR))

(check-report)
