;; pi-save-default-thinking-level!
;; 将默认思考级别写回 settings.json，保留其他设置。
;;
;; 语法
;; ----
;; (pi-save-default-thinking-level! config-dir level) -> unspecified
;;
;; 参数
;; ----
;; config-dir : string
;; JSON 配置目录。
;; level : string
;; 思考级别（off/minimal/low/medium/high/xhigh/max）。
;;
;; 返回值
;; ----
;; unspecified
;; 写入成功后的返回值未指定。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii json)
  (liii list)
  (liii string)
  (liii path)
  (liii shutil)
  (liii pi code config settings)
  (liii pi ai config record)
) ;import

(check-set-mode! 'report-failed)

(define AGENT-DIR
  (path->string (path-join (path-temp-dir) "pi-save-thinking-test"))
) ;define

(define SETTINGS (path->string (path-join AGENT-DIR "settings.json")))

(when (file-exists? AGENT-DIR)
  (rmtree AGENT-DIR #t)
) ;when
(path-mkdir AGENT-DIR :parents #t :exist-ok #t)
(path-write-text SETTINGS
  "{\"defaultProvider\":\"google\",\"defaultThinkingLevel\":\"off\",\"compaction\":{\"enabled\":false}}"
) ;path-write-text

;; Ctrl+S 只更新 defaultThinkingLevel，保留其他设置。
(pi-save-default-thinking-level! AGENT-DIR "high")
(let ((json (string->json (path-read-text SETTINGS))))
  (check (cdr (assoc "defaultThinkingLevel" json)) => "high")
  (check (cdr (assoc "defaultProvider" json)) => "google")
  (check
    (cdr (assoc "enabled" (cdr (assoc "compaction" json))))
    =>
    'false
  ) ;check
  (check (pi-settings-default-thinking-level (pi-load-settings AGENT-DIR))
    =>
    "high"
  ) ;check
  ;; 写盘为格式化 JSON（多行、2 空格缩进），不再是单行
  (check (string-contains (path-read-text SETTINGS)
           "\n  \"defaultThinkingLevel\": \"high\","
         ) ;string-contains
    =>
    #t
  ) ;check
  (check (string-contains (path-read-text SETTINGS)
           "\n  \"defaultProvider\": \"google\","
         ) ;string-contains
    =>
    #t
  ) ;check
) ;let

;; 再次保存替换旧值而不是添加重复键。
(pi-save-default-thinking-level! AGENT-DIR "low")
(let ((json (string->json (path-read-text SETTINGS))))
  (check (cdr (assoc "defaultThinkingLevel" json)) => "low")
  (check
    (length
      (filter (lambda (entry) (equal? (car entry) "defaultThinkingLevel")) json)
    ) ;length
    =>
    1
  ) ;check
) ;let

(rmtree AGENT-DIR #t)

(check-report)
