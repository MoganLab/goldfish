;; parse-emph
;; 解析单个 * 或 _ 包裹的强调内容。
;;
;; 语法
;; ----
;; (parse-emph str start)
;;
;;
;; 参数
;; ----
;; str : any
;; 输入参数。
;; start : any
;; 输入参数。
;;
;; 返回值
;; ----
;; cmark-node 或 pair 或 #f
;; 返回解析得到的节点或结构。
;; 描述
;; ----
;; emph-start? 用于判断当前位置是否可作为强调入口。

(import (liii check) (liii cmark inline parser) (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (emph-start? "*emph*" 0))
(check-true (emph-start? "_emph_" 0))
(check-false (emph-start? "text" 0))
(check-true (emph-start? "a*b" 1))

(let ((result (parse-emph "*hello*" 0)))
  (check (cmark-node-type (car result)) => 'emph)
  (check (cdr result) => 7)
) ;let

(let ((result (parse-emph "_hello_" 0)))
  (check (cmark-node-type (car result)) => 'emph)
  (check (cdr result) => 7)
) ;let

;; 未闭合时只消费起始标记 *，剩余部分由调用方继续解析
(let ((result (parse-emph "*unclosed" 0)))
  (check (cmark-node-type (car result)) => 'text)
  (check (cdr result) => 1)
) ;let

(check-report)
