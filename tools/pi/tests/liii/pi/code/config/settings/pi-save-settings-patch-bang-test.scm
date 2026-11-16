;; pi-save-settings-patch!
;; 原子更新 settings.json 中的指定字段或整个 <pi-settings> record，保留其他设置。
;;
;; 语法
;; ----
;; (pi-save-settings-patch! config-dir patch-or-settings) -> alist
;;
;; 参数
;; ----
;; config-dir : string
;; JSON 配置目录。
;; patch-or-settings : alist | pi-settings?
;; 待更新的键值 alist 或 <pi-settings> record。
;;
;; 返回值
;; ----
;; alist
;; pi-apply-writes 返回的落盘结果。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii json)
  (liii path)
  (liii shutil)
  (liii pi code config settings)
  (liii pi ai config settings)
) ;import

(check-set-mode! 'report-failed)

(define AGENT-DIR
  (path->string (path-join (path-temp-dir) "pi-save-settings-patch-test"))
) ;define

(define SETTINGS (path->string (path-join AGENT-DIR "settings.json")))

(when (file-exists? AGENT-DIR)
  (rmtree AGENT-DIR #t)
) ;when

(let ((res (pi-save-settings-patch! AGENT-DIR
             (pi-settings "deepseek" "deepseek-chat" "medium")
           ) ;pi-save-settings-patch!
      ) ;res
     ) ;
  (check-true (cdr (assoc "ok" res)))
  (check-true (file-exists? SETTINGS))
  (let ((loaded (pi-load-settings AGENT-DIR)))
    (check (pi-settings-default-provider loaded) => "deepseek")
    (check (pi-settings-default-model loaded) => "deepseek-chat")
    (check (pi-settings-default-thinking-level loaded) => "medium")
  ) ;let
) ;let

(rmtree AGENT-DIR #t)

(check-report)
