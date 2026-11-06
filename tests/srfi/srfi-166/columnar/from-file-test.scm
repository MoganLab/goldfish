;; from-file
;; 文件逐行输出组合子。
;;
;; 语法
;; ----
;; (from-file path [sep])
;;
;; 参数
;; ----
;; path : string?
;; 文本文件路径。
;;
;; sep : format?
;; 可选，行与行之间的分隔符，默认 nl。
;;
;; 返回值
;; ----
;; format?
;; 按原顺序输出文件每一行的格式化组合子。
;;
;; 说明
;; ----
;; 行本身不含换行，分隔符由 sep 提供，因此最后一行也会
;; 带一个分隔符；常与 line-numbers 组成带行号的文件列。
;; `(liii format columnar)` 以 Goldfish 命名重导出本函数。

(import (liii check)
  (scheme file)
  (srfi srfi-166 base)
  (srfi srfi-166 columnar)
) ;import

(check-set-mode! 'report-failed)

(let* ((tmp-file "tests/resources/columnar-test-temp-from-file.txt")
       (content "first line\nsecond line\nthird line")
      ) ;
  (call-with-output-file tmp-file (lambda (p) (write-string content p)))
  (check (show #f (from-file tmp-file))
    =>
    "first line\nsecond line\nthird line\n"
  ) ;check
  (check (show #f (from-file tmp-file ","))
    =>
    "first line,second line,third line,"
  ) ;check
  (check (show #f (columnar 4 'right 'infinite (line-numbers) " " (from-file tmp-file)))
    =>
    "   1 first line\n   2 second line\n   3 third line\n"
  ) ;check
  (delete-file tmp-file)
) ;let*

(check-report)
