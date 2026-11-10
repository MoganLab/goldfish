;; format-file-block
;; 将文件内容格式化为注入提示词的文件块。
;;
;; 语法
;; ----
;; (format-file-block abs-path content)
;;
;; 参数
;; ----
;; abs-path : string
;; 文件绝对路径。
;;
;; content : string
;; 文件内容。
;;
;; 返回值
;; ----
;; string
;; XML 风格的文件块文本。

(import (liii check) (liii pi code cli args files))

(check-set-mode! 'report-failed)

;; format-file-block
;; 将文件路径与内容格式化为 XML 标签包裹的文件块。
(check (format-file-block "/path/to/foo.txt" "hello world")
  =>
  "<file name=\"/path/to/foo.txt\">\nhello world\n</file>\n"
) ;check
(check (format-file-block "/path/to/bar.txt" "line 1\nline 2")
  =>
  "<file name=\"/path/to/bar.txt\">\nline 1\nline 2\n</file>\n"
) ;check

(check-report)
