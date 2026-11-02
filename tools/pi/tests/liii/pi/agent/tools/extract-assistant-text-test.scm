;; extract-assistant-text
;; 从 Assistant 响应消息中提取并合并所有的文本内容块。
;;
;; 语法
;; ----
;; (extract-assistant-text msg)
;;
;; 参数
;; ----
;; msg : list?，包含 content 字段的 Assistant 消息 alist。
;;
;; 返回值
;; ----
;; string?，由换行符连接的所有文本块内容；若无文本块则返回 ""。

(import (scheme base) (liii check) (liii pi agent tools))

(check-set-mode! 'report-failed)

;; 1. 提取单个文本块
(let ((msg
        '((role . "assistant")
          (content ((type . "text") (text . "Hello world"))))
      ) ;msg
     ) ;
  (check (extract-assistant-text msg) => "Hello world")
) ;let

;; 2. 提取多个文本块并自动用换行连接，忽略非文本块（如 toolCall）
(let ((msg
        '((role . "assistant")
          (content ((type . "text") (text . "Line 1"))
            ((type . "toolCall") (name . "bash"))
            ((type . "text") (text . "Line 2"))))
      ) ;msg
     ) ;
  (check (extract-assistant-text msg) => "Line 1\nLine 2")
) ;let

;; 3. 无 content 或 content 为空列表
(check (extract-assistant-text '((role . "assistant"))) => "")
(check (extract-assistant-text '((content))) => "")

(check-report)
