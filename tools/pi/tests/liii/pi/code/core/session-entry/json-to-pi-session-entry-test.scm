;; json->pi-session-entry
;; 将 (liii json) 解析出的 Scheme-JSON 对象转换为会话条目 record。
;;
;; 语法
;; ----
;; (json->pi-session-entry obj)
;;
;; 参数
;; ----
;; obj : any?
;; Scheme-JSON alist 或已有的 pi-session-entry record。
;;
;; 返回值
;; ----
;; any?
;; 对应的 <pi-session-*-entry> record 或 #f。

(import (liii check)
  (liii json)
  (liii pi ai message)
  (liii pi code core session-entry)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? json->pi-session-entry))

;; 1. header / message / compaction / context_edit / state round-trip
(let* ((h (pi-session-header-entry "h1" "ts0" "/tmp/work"))
       (rh (json->pi-session-entry (string->json (json->string (pi-session-entry->json h)))))
       (m (pi-session-message-entry "m1" "h1" "ts1" (pi-user-message "hi" 100)))
       (rm (json->pi-session-entry (string->json (json->string (pi-session-entry->json m)))))
       (c (pi-session-compaction-entry "c1" "m1" "ts2" "sum" "m1" 42 '((mode . "auto"))))
       (rc (json->pi-session-entry (string->json (json->string (pi-session-entry->json c)))))
       (e (pi-session-context-edit-entry "e1" "c1" "ts3" "m1" #f))
       (re (json->pi-session-entry (string->json (json->string (pi-session-entry->json e)))))
       (s (pi-session-state-entry "thinking_level_change" "s1" "e1" "ts4" '(("thinkingLevel" . "high"))))
       (rs (json->pi-session-entry (string->json (json->string (pi-session-entry->json s)))))
      ) ;
  (check-true (pi-session-header-entry? rh))
  (check (pi-session-header-entry-cwd rh) => "/tmp/work")
  (check-true (pi-session-message-entry? rm))
  (check-true (pi-user-message? (pi-session-message-entry-message rm)))
  (check-true (pi-session-compaction-entry? rc))
  (check (pi-session-compaction-entry-summary rc) => "sum")
  (check-true (pi-session-context-edit-entry? re))
  (check-false (pi-session-context-edit-entry-replacement re))
  (check-true (pi-session-state-entry? rs))
  (check (pi-session-state-entry-type rs) => "thinking_level_change")
) ;let*

;; 2. 幂等与无效输入
(let ((h (pi-session-header-entry "h1" "ts0" "/tmp")))
  (check (json->pi-session-entry h) => h)
) ;let
(check-false (json->pi-session-entry '()))
(check-catch 'type-error (json->pi-session-entry "bad"))
(check-catch 'type-error (json->pi-session-entry 123))

(check-report)
