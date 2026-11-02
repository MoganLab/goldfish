;; pi-agent-assoc-val
;; 在 alist 中安全按主键或备选键查询对应的值，并在未命中时返回默认值。
;;
;; 语法
;; ----
;; (pi-agent-assoc-val key alt-key obj default-val)
;;
;; 参数
;; ----
;; key : any?，主键名（如符号 'content）。
;; alt-key : any?，备选键名（如字符串 "content"）。
;; obj : list?，待查询的 alist 对象。
;; default-val : any?，未找到键时的兜底默认值。
;;
;; 返回值
;; ----
;; any?，匹配项的 cdr 值；若未匹配则返回 default-val。

(import (scheme base) (liii check) (liii pi agent tools))

(check-set-mode! 'report-failed)

;; 1. 命中主键（symbol）
(let ((alist '((type . "text") ("type" . "alt-text"))))
  (check (pi-agent-assoc-val 'type "type" alist "") => "text")
) ;let

;; 2. 命中备选键（string）
(let ((alist '(("type" . "toolCall"))))
  (check (pi-agent-assoc-val 'type "type" alist "") => "toolCall")
) ;let

;; 3. 两个键均未命中：返回兜底默认值
(let ((alist '((other . 123))))
  (check (pi-agent-assoc-val 'type "type" alist "default") => "default")
) ;let

;; 4. 非 pair 或空 alist：返回兜底默认值
(check (pi-agent-assoc-val 'type "type" '() #f) => #f)

(check-report)
