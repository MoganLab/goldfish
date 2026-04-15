(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; eof-object
;; 返回文件结束对象。
;;
;; 语法
;; ----
;; (eof-object)
;;
;; 返回值
;; ------
;; 文件结束对象（end-of-file object）。
;; 测试 eof-object 返回值
(check (eof-object) => #<eof>)
;; 测试从空输入读取返回 eof-object
(let ((p (open-input-string "")))
  (check (read p) => #<eof>)
  (close-port p)
) ;let
;; 测试多次读取空输入
(let ((p (open-input-string "")))
  (check (read p) => #<eof>)
  (check (read p) => #<eof>)
  (close-port p)
) ;let
(check-report)