;; read-all-lines
;; 读取指定文件的全部文本行，以字符串列表形式返回。
;;
;; 语法
;; ----
;; (read-all-lines file-path)
;;
;; 参数
;; ----
;; file-path : string   待读取的文件路径
;;
;; 返回值
;; ------
;; list of string
;; 文件的每一行字符串组成的列表。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii goldcheck unit-test file)
) ;import

(check-set-mode! 'report-failed)

;; 1. 读取包含多行内容的文件
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-read-all-lines.txt")))
       (content "line1\nline2\nline3\n")
      ) ;
  (call-with-output-file tmp (lambda (p) (write-string content p)))
  (check (read-all-lines tmp) => '("line1" "line2" "line3"))
  (path-unlink (path tmp))
) ;let*

(check-report)
