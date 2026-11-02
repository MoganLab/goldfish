;; pi-extract-tool-result-text
;; 从工具执行结果结构中提取首个块的文本内容。
;;
;; 语法
;; ----
;; (pi-extract-tool-result-text res)
;;
;; 参数
;; ----
;; res : list?，包含 content 字段的工具结果 alist。
;;
;; 返回值
;; ----
;; string?，提取出的结果文本，若无内容或格式不匹配则返回 ""。

(import (scheme base) (liii check) (liii pi agent tools))

(check-set-mode! 'report-failed)

;; 1. 从向量 content 中提取 text
(let ((res
        (list
          (cons 'content (vector (list (cons 'type "text") (cons 'text "output-abc"))))
        ) ;list
      ) ;res
     ) ;
  (check (pi-extract-tool-result-text res) => "output-abc")
) ;let

;; 2. 从列表 content 中提取 text
(let ((res (list (cons "content" (list (list (cons "text" "output-def")))))))
  (check (pi-extract-tool-result-text res) => "output-def")
) ;let

;; 3. content 为空或无 text 属性返回空字符串 ""
(check (pi-extract-tool-result-text '((content . #()))) => "")
(check (pi-extract-tool-result-text '()) => "")

(check-report)
