;; parse-position-file
;; 读取解析位置记录的文件名。
;;
;; 语法
;; ----
;; (parse-position-file pos)
;;
;; 参数
;; ----
;; pos : <parse-position>
;; 解析位置记录。
;;
;; 返回值
;; ----
;; string 或 #f
;; 源文件名；未知时为 #f。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(check (parse-position-file (make-parse-position "test.scm" 3 15)) => "test.scm")
(check-false (parse-position-file (make-parse-position #f 1 0)))

(check-report)
