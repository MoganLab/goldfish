;; make-wheel-scroll-accelerator
;; 创建一个鼠标滚轮滚动加速器实例。
;;
;; 语法
;; ----
;; (make-wheel-scroll-accelerator [:lines lines])
;;
;; 参数
;; ----
;; lines : symbol? 或 integer? (可选，默认为 'auto)
;; 滚动模式。可为 'auto（根据滚动事件时间间隔自动加速，最多 6 行）或正整数（固定单次滚动行数）。
;;
;; 返回值
;; ----
;; <wheel-scroll-accelerator>
;; 滚动加速器实例。

(import (liii check) (liii tui wheel-scroll))

(check-set-mode! 'report-failed)

(let ((acc (make-wheel-scroll-accelerator)))
  (check (wheel-scroll-accelerator? acc) => #t)
  (check (wheel-scroll-lines acc) => 'auto)
  (check-catch 'type-error (make-wheel-scroll-accelerator :lines 0))
  (check-catch 'type-error (make-wheel-scroll-accelerator :lines "x"))
  (check-catch 'type-error (make-wheel-scroll-accelerator :bogus 1))
) ;let

(check-report)
