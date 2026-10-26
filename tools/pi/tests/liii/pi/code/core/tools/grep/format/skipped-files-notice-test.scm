(import (liii check)
  (liii pi code core tools grep format)
) ;import

(check-set-mode! 'report-failed)

;; %skipped-files-notice：读不出来的文件必须被明确告知

;; 1. 单个文件
(check (%skipped-files-notice '("a/b.txt"))
       => "1 files skipped (unreadable or removed). First: a/b.txt")

;; 2. 多个文件时给出数量与第一个路径
(check (%skipped-files-notice '("a/b.txt" "c/d.txt"))
       => "2 files skipped (unreadable or removed). First: a/b.txt")

(check-report)
