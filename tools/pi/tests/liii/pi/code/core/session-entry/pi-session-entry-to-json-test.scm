;; pi-session-entry->json
;; 将会话条目 record 转换为符合 (liii json) 规范的 Scheme-JSON 对象。
;;
;; 语法
;; ----
;; (pi-session-entry->json entry)
;;
;; 参数
;; ----
;; entry : any?
;; <pi-session-*-entry> record。
;;
;; 返回值
;; ----
;; any?
;; 可直接用于 json->string 或 json-ref 的 Scheme-JSON 对象。

(import (liii check)
  (liii json)
  (liii pi ai message)
  (liii pi code core session-entry)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? pi-session-entry->json))

;; 1. header entry
(let ((j (pi-session-entry->json (pi-session-header-entry "h1" "2026-01-01T00:00:00Z" "C:\\work"))))
  (check (json-ref j "type") => "session")
  (check (json-ref j "version") => 3)
  (check (json-ref j "id") => "h1")
  (check (json-ref j "cwd") => "C:\\work")
  (check (string->json (json->string j)) => j)
) ;let

;; 2. message entry 嵌套 pi-user-message
(let ((j (pi-session-entry->json
           (pi-session-message-entry "m1" #f "2026-01-01T00:00:01Z" (pi-user-message "hi" 100))
         ) ;pi-session-entry->json
      ) ;j
     ) ;
  (check (json-ref j "type") => "message")
  (check (json-ref j "id") => "m1")
  (check (json-ref j "parentId") => 'null)
  (check (json-ref j "message" "role") => "user")
  (check (json-ref j "message" "content") => "hi")
) ;let

;; 3. compaction / context_edit / state entry
(let ((jc (pi-session-entry->json (pi-session-compaction-entry "c1" "m1" "ts" "sum" "m1" 500 '())))
      (je (pi-session-entry->json (pi-session-context-edit-entry "e1" "c1" "ts" "m1" #f)))
      (js (pi-session-entry->json (pi-session-state-entry "model_change" "s1" "e1" "ts" '(("provider" . "google") ("modelId" . "gemini")))))
     ) ;
  (check (json-ref jc "type") => "compaction")
  (check (json-ref jc "firstKeptEntryId") => "m1")
  (check (json-ref jc "details") => '(()))
  (check (json-ref je "type") => "context_edit")
  (check (json-ref je "replacement") => 'null)
  (check (json-ref js "type") => "model_change")
  (check (json-ref js "provider") => "google")
) ;let

(check-catch 'type-error (pi-session-entry->json '(("type" . "session"))))
(check-catch 'type-error (pi-session-entry->json "bad"))

(check-report)
