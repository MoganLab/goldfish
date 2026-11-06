;; %word-left-boundary
;; 纯函数计算文本中光标向左跳单词的边界字符索引。
;;
;; 语法
;; ----
;; (%word-left-boundary line col)
;;
;; 参数
;; ----
;; line : string?
;; 当前文本行。
;;
;; col : integer?
;; 当前列位置。
;;
;; 返回值
;; ----
;; integer?
;; 单词左边界的字符索引。

(import (scheme base) (liii check) (liii tui editor cursor))

(check-set-mode! 'report-failed)

(check (%word-left-boundary "hello world" 11) => 6)
(check (%word-left-boundary "hello world" 6) => 0)
(check (%word-left-boundary "hello" 0) => 0)

(check-report)
