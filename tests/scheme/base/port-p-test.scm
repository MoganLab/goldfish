(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; port?
;; 检查对象是否为端口（输入或输出端口）。
;;
;; 语法
;; ----
;; (port? obj)
;;
;; 参数
;; ----
;; obj : 任意对象
;; 要检查的对象。
;;
;; 返回值
;; ------
;; boolean?
;; 如果对象是端口返回 #t，否则返回 #f。

;; 测试输入端口
(let ((p (open-input-string "hello")))
  (check (port? p) => #t)
  (check (input-port? p) => #t)
  (check (output-port? p) => #f)
  (close-port p))

;; 测试输出端口
(let ((p (open-output-string)))
  (check (port? p) => #t)
  (check (input-port? p) => #f)
  (check (output-port? p) => #t)
  (close-port p))

;; 测试非端口对象
(check (port? 123) => #f)
(check (port? "string") => #f)
(check (port? 'symbol) => #f)
(check (port? '(list)) => #f)
(check (port? #t) => #f)
(check (port? #\a) => #f)

;; 测试 binary-port? 和 textual-port?
(let ((p (open-input-string "test")))
  (check (binary-port? p) => #t)
  (check (textual-port? p) => #t)
  (close-port p))

(check-report)
