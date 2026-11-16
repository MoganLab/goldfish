;; %string-key-json-object?
;; 判断输入是否为字符串键的 JSON 对象（含空对象 '(())）。
;;
;; 语法
;; ----
;; (%string-key-json-object? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 输入对象。
;;
;; 返回值
;; ----
;; boolean?
;; 字符串键 JSON 对象返回 #t，否则返回 #f。

(import (liii check) (liii pi ai message block))
(check-set-mode! 'report-failed)

(check-true (%string-key-json-object? '(())))
(check-true (%string-key-json-object? '(("type" . "text"))))
(check-false (%string-key-json-object? '((type . "text"))))
(check-false (%string-key-json-object? '()))
(check-false (%string-key-json-object? "text"))

(check-report)
