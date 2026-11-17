;; pi-config-aio-enter
;; 在 all-in-one 子标签页按 Enter 将当前选中的链条设为默认模型。
;;
;; 语法
;; ----
;; (pi-config-aio-enter st cursor)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;; cursor : list
;; 当前光标。
;;
;; 返回值
;; ------
;; string
;; 操作结果提示消息。

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
    (string-append "pi-cfg-aio-enter-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir (path-join TMP-DIR "models") :parents #t :exist-ok #t)

;; 1. all-in-one 未启用时提示先启用
(path-write-text (path-join TMP-DIR "models" "providers.json") "[\"google\"]")
(let* ((st (pi-config-state (path->string TMP-DIR) :env '()))
       (msg (pi-config-aio-enter st '((tab . 1) (chain . 0) (node . 0))))
      ) ;
  (check (string-contains? msg "disabled") => #t)
) ;let*

;; 2. all-in-one 已启用时写入 settings.json 默认模型
(path-write-text (path-join TMP-DIR "models" "providers.json")
  "[\"google\",\"all-in-one\"]"
) ;path-write-text
(let* ((st (pi-config-state (path->string TMP-DIR) :env '()))
       (msg (pi-config-aio-enter st '((tab . 1) (chain . 1) (node . 0))))
       (settings (path-read-text (path-join TMP-DIR "settings.json")))
      ) ;
  (check (string-contains? msg "all-in-one/coding-high-quality") => #t)
  (check (string-contains? settings "\"defaultProvider\": \"all-in-one\"") => #t)
  (check (string-contains? settings "\"defaultModel\": \"coding-high-quality\"")
    =>
    #t
  ) ;check
) ;let*

(rmtree (path->string TMP-DIR))

(check-report)
