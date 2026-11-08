;; fitted
;; 定宽组合子：内容不足时填充，超出时裁剪到精确宽度。
;;
;; 语法
;; ----
;; (fitted width . ls)
;; (fitted/right width . ls)
;; (fitted/both width . ls)
;;
;; 参数
;; ----
;; width : exact-integer?
;; 目标显示宽度。
;;
;; ls : format?
;; 零个或多个格式化对象。
;;
;; 返回值
;; ----
;; format?
;; 输出恰好占 width 列的格式化组合子。
;;
;; 说明
;; ----
;; padded 只填充不裁剪，trimmed 只裁剪不填充，fitted 两者都做：
;; fitted 左侧填充，fitted/right 右侧填充，
;; fitted/both 两侧均分填充；超出宽度时裁剪到 width 列。
;; `(liii format base)` 以 Goldfish 命名重导出这些函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (fitted 5 "abc")) => "  abc")
(check (show #f (fitted 5 "abcdefgh")) => "defgh")
(check (show #f (fitted 0 "abc")) => "")

(check-report)
