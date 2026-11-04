;; format-nodes
;; 格式化一组 node 树，并返回拼接后的文本。
;;
;; 语法
;; ----
;; (format-nodes nodes)
;;
;; 参数
;; ----
;; nodes : vector?
;; 待格式化的 node 树集合，每个元素对应一个顶层表达式。
;;
;; 返回值
;; ----
;; string?
;; 格式化后的文本，各个顶层表达式的结果以换行连接。
;;
;; 说明
;; ----
;; 1. `format-nodes` 不做扫描，直接对传入的 node 树排版
;; 2. 顶层 define 之间会自动插入一个空行
;; 3. 输出始终以换行符结尾

(import (liii check)
  (liii goldfmt format)
  (liii goldfmt scan)
  (liii goldfmt record)
  (liii string)
) ;import

(check-set-mode! 'report-failed)

;; 测试：单个顶层表达式
(let ((text (format-nodes (scan-source-string "(define x 1)\n"))))
  (check (string? text) => #t)
  (check (string-contains? text "(define x 1)") => #t)
) ;let

;; 测试：多个顶层表达式之间用换行连接
(let ((text (format-nodes (scan-source-string "(define x 1)\n(define y 2)\n"))))
  (check (string-contains? text "(define x 1)") => #t)
  (check (string-contains? text "(define y 2)") => #t)
) ;let

;; 测试：输出始终以换行符结尾
(let ((text (format-nodes (scan-source-string "(define x 1)\n"))))
  (check (equal? (substring text (- (string-length text) 1) (string-length text)) "\n") => #t)
) ;let

;; 测试：空的 node 集合返回空字符串
(check (format-nodes (vector)) => "")

(check-report)
