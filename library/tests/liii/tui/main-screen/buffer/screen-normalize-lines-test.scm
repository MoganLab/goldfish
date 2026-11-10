;; screen-normalize-lines
;; 将渲染目标规范化为文本行列表，展开内嵌换行符。
;;
;; 语法
;; ----
;; (screen-normalize-lines target caller)
;;
;; 参数
;; ----
;; target : component? 或 string? 或 list?
;; 渲染目标。组件返回 #f，字符串按换行符切分，行列表逐行展开内嵌换行。
;;
;; caller : string
;; 调用方名称，用于错误信息。
;;
;; 返回值
;; ----
;; list? 或 #f
;; 规范化后的文本行列表；若 target 为组件则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui main-screen buffer)
) ;import

(check-set-mode! 'report-failed)

;; 无内嵌换行时保持原列表与字符串对象，避免逐帧重建规范化结果。
(let ((lines (list "first" "第二行" "")))
  (check (eq? (screen-normalize-lines lines "test") lines) => #t)
  (check (eq? (screen-normalize-lines lines "test") lines) => #t)
) ;let

;; 有内嵌换行时仍按原语义逐行展开，并保留尾部空行。
(let ((lines (list "first" "second\nthird" "fourth\n")))
  (check (screen-normalize-lines lines "test")
    =>
    '("first" "second" "third" "fourth" "")
  ) ;check
) ;let

;; 字符串目标
(check (screen-normalize-lines "one\ntwo" "test") => '("one" "two"))
(check (screen-normalize-lines "unchanged" "test") => '("unchanged"))

;; 组件目标返回 #f
(check (screen-normalize-lines (make-text "hello") "test") => #f)

;; 类型错误
(check-catch 'type-error (screen-normalize-lines '("ok" 1) "test"))

(check-report)
