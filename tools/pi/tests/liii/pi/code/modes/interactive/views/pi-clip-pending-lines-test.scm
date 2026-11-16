;; pi-clip-pending-lines
;; 按终端高度预算裁剪待发送队列行，防止队列过长遮挡整个 TUI 界面。
;;
;; 语法
;; ----
;; (pi-clip-pending-lines lines rows footer-count menu-count width)
;;
;; 参数
;; ----
;; lines : list?
;; 待发送队列渲染后的行列表。
;; rows : integer?
;; 终端总行数。
;; footer-count : integer?
;; 页脚占用行数。
;; menu-count : integer?
;; 菜单占用行数。
;; width : integer?
;; 终端宽度。
;;
;; 返回值
;; ------
;; list?
;; 返回裁剪后的行列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii tui)
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

;; 未超限时原样保留
(let ((lines '("  a" "  b" "  c")))
  (check (pi-clip-pending-lines lines 24 1 0 80) => lines)
) ;let

;; 24 行终端下最多保留 8 行（前 7 行 + 1 行省略提示）
(let* ((lines (map (lambda (i) (string-append "  line " (number->string i))) (iota 20)))
       (clipped (pi-clip-pending-lines lines 24 1 0 80))
      ) ;
  (check (length clipped) => 8)
  (check (strip-ansi (list-ref clipped 7)) => "  ... (+13 more lines)")
) ;let*

;; 极矮终端下优先保证对话区与输入区，队列可裁剪到 0 或 1 行
(let ((lines '("  a" "  b" "  c")))
  (check (pi-clip-pending-lines lines 5 1 0 80) => '())
  (let ((one (pi-clip-pending-lines lines 6 1 0 80)))
    (check (length one) => 1)
    (check (strip-ansi (car one)) => "  ... (3 lines queued)")
  ) ;let
) ;let

(check-report)
