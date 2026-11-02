;; highlight-selection-lines
;; 为多行文本按给定的选区范围注入反显控制序列。
;;
;; 语法
;; ----
;; (highlight-selection-lines lines sel)
;;
;; 参数
;; ----
;; lines : (list-of string?)
;; 渲染后的行文本列表。
;; sel : selection-range?
;; 选区范围对象。
;;
;; 返回值
;; -----
;; (list-of string?)
;; 包含反显控制序列的高亮后行列表。

(import (scheme base) (liii check) (liii pi code modes interactive selection))

(check-set-mode! 'report-failed)

;; 1. 多行选区包裹：首行、中间行、末行与未选中行
(let* ((lines (list "line 0" "line 1" "line 2" "line 3"))
       (sel (make-selection-range '(2 . 1) '(3 . 2) #t))
       (res (highlight-selection-lines lines sel))
      ) ;
  ;; line 0 不受影响
  (check (list-ref res 0) => "line 0")
  ;; line 1 从列 2 选到行末
  (check (list-ref res 1) => "li\x1b;[7mne 1\x1b;[27m")
  ;; line 2 从行首选到列 3
  (check (list-ref res 2) => "\x1b;[7mline\x1b;[27m 2")
  ;; line 3 不受影响
  (check (list-ref res 3) => "line 3")
) ;let*

;; 2. 未激活选区不修改任何行
(let* ((lines (list "hello" "world"))
       (sel (make-selection-range '(0 . 0) '(3 . 0) #f))
      ) ;
  (check (highlight-selection-lines lines sel) => lines)
) ;let*

(check-report)
