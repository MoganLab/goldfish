;; extract-result-text
;; 从工具执行结果中提取出可读的文本字符串。
;;
;; 语法
;; ----
;; (extract-result-text res)
;;
;; 参数
;; ----
;; res : any
;; 工具返回的原始结果（可能是 string、vector、alist、#f 等）。
;;
;; 返回值
;; ----
;; string?
;; 提取出的纯文本结果。

(import (scheme base) (liii check) (liii pi code modes interactive renderers))

(check-set-mode! 'report-failed)

;; 1. 字符串直接返回
(check (extract-result-text "hello world") => "hello world")

;; 2. #f 返回空字符串
(check (extract-result-text #f) => "")

;; 3. vector 递归提取首元素
(check (extract-result-text #("result from vector")) => "result from vector")

;; 4. alist 提取 text 键
(check (extract-result-text '((text . "extracted text"))) => "extracted text")

(check-report)
