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
