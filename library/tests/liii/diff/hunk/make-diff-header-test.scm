;; make-diff-header
;; 生成统一差异（Unified Diff）头部两行。
;;
;; 语法
;; ----
;; (make-diff-header fromfile tofile fromdate todate)
;;
;; 参数
;; ----
;; fromfile : string
;; 原文件名标签。
;; tofile : string
;; 新文件名标签。
;; fromdate : string
;; 原文件时间标签，空字符串表示不输出时间。
;; todate : string
;; 新文件时间标签，空字符串表示不输出时间。
;;
;; 返回值
;; ----
;; list
;; 含两个字符串的列表：原文件头与新文件头；
;; 时间标签非空时以制表符分隔追加在文件名之后。

(import (liii check)
        (liii diff hunk))

(check-set-mode! 'report-failed)

;; 无时间标签时只输出文件名
(check (make-diff-header "a" "b" "" "") => '("--- a" "+++ b"))

;; 有时间标签时以制表符分隔追加
(check (make-diff-header "old.py" "new.py" "2024-01-01" "2024-01-02")
  => '("--- old.py\t2024-01-01" "+++ new.py\t2024-01-02"))

;; 只有一侧有时间标签时各自独立处理
(check (make-diff-header "a" "b" "d1" "")
  => '("--- a\td1" "+++ b"))
(check (make-diff-header "a" "b" "" "d2")
  => '("--- a" "+++ b\td2"))

(check-report)
