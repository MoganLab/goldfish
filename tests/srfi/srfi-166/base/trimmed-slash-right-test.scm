;; trimmed/right
;; 右侧截断组合子：内容超宽时保留头部。
;;
;; 语法
;; ----
;; (trimmed/right width . ls)
;;
;; 参数
;; ----
;; width : exact-integer?
;; 允许的最大显示宽度。
;;
;; ls : format?
;; 零个或多个格式化对象。
;;
;; 返回值
;; ----
;; format?
;; 输出不超过 width 列、保留头部内容的格式化组合子。
;;
;; 说明
;; ----
;; 状态变量 ellipsis 非空时，被裁掉的尾部用省略号补位并计入
;; width。保留尾部请用 trimmed，保留中间请用 trimmed/both。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (trimmed/right 3 "abcdef")) => "abc")
(check (show #f (trimmed/right 10 "abc")) => "abc")
(check
  (show #f (with ((ellipsis "...")) (trimmed/right 5 "abcdef")))
  =>
  "ab..."
) ;check

(check-report)
