;; prepare-system-prompt
;; 构造系统提示词：合并 CLI 覆盖、追加提示、项目上下文与会话预算。
;;
;; 语法
;; ----
;; (prepare-system-prompt cfg parsed)
;;
;; 参数
;; ----
;; cfg : alist
;; 运行配置 alist，含 tools（工具名列表）。
;; parsed : alist
;; 解析后的命令行参数 alist；支持 system-prompt、append-system-prompt、
;; max-turns 与 session-timeout。
;;
;; 返回值
;; ----
;; string
;; 完整的系统提示词文本。

(import (scheme base)
  (liii check)
  (liii string)
  (liii pi code modes flow)
) ;import

(check-set-mode! 'report-failed)

(define CFG (list (cons 'tools '("read" "bash"))))

;; 1. 默认提示词：含工具、cwd 与会话预算段落
(let ((prompt (prepare-system-prompt CFG '())))
  (check-true (string-contains? prompt "<tools>"))
  (check-true (string-contains? prompt "<cwd>"))
  (check-true (string-contains? prompt "<session_budget>"))
  (check-true (string-contains? prompt "Session Timeout: None"))
  (check-true (string-contains? prompt "Max Turns: Unlimited")))

;; 2. --system-prompt 覆盖：仅保留覆盖文本，不再拼接工具段落
(let ((prompt (prepare-system-prompt CFG (list (cons 'system-prompt "CUSTOM PROMPT")))))
  (check-true (string-contains? prompt "CUSTOM PROMPT"))
  (check-false (string-contains? prompt "<tools>")))

;; 3. --append-system-prompt：追加 addendum 段落
(let ((prompt (prepare-system-prompt CFG (list (cons 'append-system-prompt '("EXTRA ONE"))))))
  (check-true (string-contains? prompt "<addendum>"))
  (check-true (string-contains? prompt "EXTRA ONE")))

;; 4. 会话预算：session-timeout 与 max-turns 生效
(let ((prompt (prepare-system-prompt CFG
                (list (cons 'session-timeout 30) (cons 'max-turns 5)))))
  (check-true (string-contains? prompt "Session Timeout: 30 seconds"))
  (check-true (string-contains? prompt "Max Turns: 5 turns")))

(check-report)
