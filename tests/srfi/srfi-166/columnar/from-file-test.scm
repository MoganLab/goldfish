(import (liii check)
        (scheme file)
        (srfi srfi-166 base)
        (srfi srfi-166 columnar))

(check-set-mode! 'report-failed)

;; from-file
;; 从文件中读取并逐行输出内容的组合子。
;;
;; 语法
;; ----
;; (from-file path [sep])
;;
;; 示例
;; ----
;; (show #f (from-file "test.txt"))

(let* ((tmp-file "tests/resources/test-columnar-from-file.txt")
       (content "first line\nsecond line\nthird line"))
  (call-with-output-file tmp-file
    (lambda (p) (write-string content p)))
  (check (show #f (from-file tmp-file)) => "first line\nsecond line\nthird line\n")
  (check (show #f (columnar 4 'right 'infinite (line-numbers) " " (from-file tmp-file)))
         => "   1 first line\n   2 second line\n   3 third line\n")
  (delete-file tmp-file))

(check-report)
