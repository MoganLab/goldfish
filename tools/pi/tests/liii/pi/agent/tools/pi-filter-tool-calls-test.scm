;; pi-filter-tool-calls
;; 从消息 content 列表中筛选过滤出所有类型为 toolCall 的工具调用块。
;;
;; 语法
;; ----
;; (pi-filter-tool-calls content)
;;
;; 参数
;; ----
;; content : list?，包含各类内容块（如 text、toolCall、image 等）的列表。
;;
;; 返回值
;; ----
;; list?，仅包含 toolCall 块的列表。

(import (scheme base) (liii check) (liii pi agent tools))

(check-set-mode! 'report-failed)

;; 1. 混合块中筛选出所有 toolCall 块
(let* ((call1 '((type . "toolCall") (id . "c1") (name . "bash")))
       (txt '((type . "text") (text . "thinking...")))
       (call2 '(("type" . "toolCall") ("id" . "c2") ("name" . "read")))
       (content (list call1 txt call2))
       (res (pi-filter-tool-calls content))
      ) ;
  (check (length res) => 2)
  (check (car res) => call1)
  (check (cadr res) => call2)
) ;let*

;; 2. 无 toolCall 块时返回空列表
(let ((content (list '((type . "text") (text . "hello")))))
  (check (pi-filter-tool-calls content) => '())
) ;let

;; 3. 空输入返回空列表
(check (pi-filter-tool-calls '()) => '())

(check-report)
