;; pi-config-aio-enabled-targets
;; 获取所有已启用的非 all-in-one 供应商及其已启用的模型列表。
;;
;; 语法
;; ----
;; (pi-config-aio-enabled-targets st)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;;
;; 返回值
;; ------
;; list
;; ((provider-id . (model ...)) ...) 列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii os)
  (liii path)
  (liii shutil)
  (liii pi code modes config state)
  (liii pi code modes config all-in-one)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path-join (path-temp-dir)
    (string-append "pi-cfg-aio-targets-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir (path-join TMP-DIR "models") :parents #t :exist-ok #t)
(path-write-text (path-join TMP-DIR "models" "providers.json")
  "[\"google\",\"all-in-one\"]"
) ;path-write-text
(path-write-text (path-join TMP-DIR "models" "google.json")
  "{\"models\":[\"gemini-3.8-flash\"]}"
) ;path-write-text

(let* ((st (pi-config-state (path->string TMP-DIR) :env '()))
       (targets (pi-config-aio-enabled-targets st))
      ) ;
  (check (length targets) => 1)
  (check (caar targets) => "google")
  (check (length (cdar targets)) => 1)
  (check (pi-config-model-id (car (cdar targets))) => "gemini-3.8-flash")
) ;let*

(rmtree (path->string TMP-DIR))

(check-report)
