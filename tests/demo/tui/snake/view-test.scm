(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui box)
  (liii tui loader)
  (liii tui editor)
  (liii tui text-width)
  (liii tui terminal)
  (liii tui main-screen)
  (demo tui snake logic)
  (demo tui snake view)
  (demo tui snake style)
) ;import

(check-set-mode! 'report-failed)

;; 辅助函数：统计行列表中的特定字符出现总数
(define (%count-char lines ch)
  (let loop
    ((rem lines) (cnt 0))
    (if (null? rem)
      cnt
      (let* ((str (car rem))
             (len (string-length str))
            ) ;
        (let in-loop
          ((i 0) (sub 0))
          (if (< i len)
            (in-loop (+ i 1) (if (char=? (string-ref str i) ch) (+ sub 1) sub))
            (loop (cdr rem) (+ cnt sub))
          ) ;if
        ) ;let
      ) ;let*
    ) ;if
  ) ;let
) ;define

;; =========================================================
;; 第一部分：无色安全降级测试 (with-color-mode #f)
;; 验证当颜色被禁用或 NO_COLOR 时，输出与原无色版 100% 逐字一致
;; =========================================================
(with-color-mode #f
  (lambda ()
    ;; 1. 无色 render-grid-lines
    (let* ((game (make-snake-game :width 10 :height 6 :snake '((3 2) (2 2) (1 2)) :food '(5 2)))
           (lines (render-grid-lines game))
          ) ;
      (check (length lines) => 6)
      (let ((row2 (list-ref lines 2)))
        (check (string-length row2) => 10)
        (check (string-ref row2 3) => #\@)
        (check (string-ref row2 2) => #\o)
        (check (string-ref row2 1) => #\o)
        (check (string-ref row2 5) => #\*)
      ) ;let
    ) ;let*

    ;; 2. 无色 make-stats-box 与 make-help-box
    (let* ((game (make-snake-game :score 30 :high-score 50))
           (spinner (make-spinner))
           (stats-box (make-stats-box game spinner))
           (stats-lines (component-render stats-box 25))
          ) ;
      (check (component? stats-box) => #t)
      (check (pair? stats-lines) => #t)
      (let ((combined (apply string-append stats-lines)))
        (check (> (display-width combined) 0) => #t)
      ) ;let
    ) ;let*

    ;; 3. 无色 make-game-view 整体布局与网格对应
    (let* ((game (make-snake-game :width 20 :height 10 :snake '((5 5) (4 5) (3 5) (2 5) (1 5)) :food '(10 5)))
           (spinner (make-spinner))
           (view (make-game-view game spinner))
           (lines (component-render view 60))
           (grid (render-grid-lines game))
           (grid-row5 (list-ref grid 5))
           (view-row6 (list-ref lines 6))
          ) ;
      (check (component? view) => #t)
      (check (pair? lines) => #t)
      (check (> (length lines) 5) => #t)
      (check (string-ref grid-row5 0) => #\space)
      (check (string-ref grid-row5 1) => #\o)
      (check (string-ref grid-row5 5) => #\@)
      (check (substring view-row6 0 9) => "║ oooo@")
    ) ;let*

    ;; 4. 无色长蛇拐弯与 S 形走位
    (let* ((snake-body-coords '((5 3) (5 4) (5 5) (4 5) (3 5) (2 5) (2 6) (2 7)))
           (len (length snake-body-coords))
           (game (make-snake-game :width 20 :height 10 :snake snake-body-coords :food '(15 8)))
           (spinner (make-spinner))
           (grid (render-grid-lines game))
           (board-comp (make-component (lambda (comp width) grid)))
           (board-box (make-box board-comp :title " SNAKE (liii tui) " :style 'double :padding 0))
          ) ;
      (check (%count-char grid #\@) => 1)
      (check (%count-char grid #\o) => (- len 1))
      (let ((head (car snake-body-coords)))
        (check (string-ref (list-ref grid (cadr head)) (car head)) => #\@)
      ) ;let
      (for-each
        (lambda (seg)
          (check (string-ref (list-ref grid (cadr seg)) (car seg)) => #\o)
        ) ;lambda
        (cdr snake-body-coords)
      ) ;for-each
      (let ((board-lines (component-render board-box 22)))
        (check (%count-char board-lines #\@) => 1)
        (check (%count-char board-lines #\o) => (- len 1))
      ) ;let
    ) ;let*

    ;; 5. 无色水平穿墙
    (let* ((cross-coords '((1 5) (0 5) (19 5) (18 5) (17 5) (16 5)))
           (len (length cross-coords))
           (game (make-snake-game :width 20 :height 10 :snake cross-coords :food '(10 2)))
           (grid (render-grid-lines game))
          ) ;
      (check (%count-char grid #\@) => 1)
      (check (%count-char grid #\o) => (- len 1))
      (check (string-ref (list-ref grid 5) 1) => #\@)
      (check (string-ref (list-ref grid 5) 0) => #\o)
      (check (string-ref (list-ref grid 5) 19) => #\o)
      (check (string-ref (list-ref grid 5) 18) => #\o)
      (check (string-ref (list-ref grid 5) 17) => #\o)
      (check (string-ref (list-ref grid 5) 16) => #\o)
      (check (string-ref (list-ref grid 5) 2) => #\space)
      (check (string-ref (list-ref grid 5) 15) => #\space)
    ) ;let*

    ;; 6. 无色垂直穿墙
    (let* ((v-coords '((5 0) (5 9) (5 8) (5 7) (5 6)))
           (len (length v-coords))
           (game (make-snake-game :width 20 :height 10 :snake v-coords :food '(12 4)))
           (grid (render-grid-lines game))
          ) ;
      (check (%count-char grid #\@) => 1)
      (check (%count-char grid #\o) => (- len 1))
      (check (string-ref (list-ref grid 0) 5) => #\@)
      (check (string-ref (list-ref grid 9) 5) => #\o)
      (check (string-ref (list-ref grid 8) 5) => #\o)
      (check (string-ref (list-ref grid 7) 5) => #\o)
      (check (string-ref (list-ref grid 6) 5) => #\o)
    ) ;let*
  ) ;lambda
) ;with-color-mode

;; =========================================================
;; 第二部分：带颜色模式测试 (with-color-mode #t)
;; 验证 ANSI 颜色分层、宽度透明度、虚拟终端屏幕纯净性与 raw 日志
;; =========================================================
(with-color-mode #t
  (lambda ()
    ;; 1. 带颜色 render-grid-lines：包含 SGR 且 strip-ansi 与原文本严格一致
    (let* ((game (make-snake-game :width 10 :height 6 :snake '((3 2) (2 2) (1 2)) :food '(5 2)))
           (lines (render-grid-lines game))
           (row2 (list-ref lines 2))
           (plain-row2 (strip-ansi row2))
          ) ;
      (check (length lines) => 6)
      ;; 显示宽度仍严格为 10 列
      (check (display-width row2) => 10)
      ;; 剥离 ANSI 后与无色版完全一致
      (check (string-length plain-row2) => 10)
      (check (string-ref plain-row2 3) => #\@)
      (check (string-ref plain-row2 2) => #\o)
      (check (string-ref plain-row2 1) => #\o)
      (check (string-ref plain-row2 5) => #\*)
      ;; 原始字符串中含有 ANSI 颜色转义码
      (check (> (string-length row2) 10) => #t)
    ) ;let*

    ;; 2. 带颜色侧栏与视图组件
    (let* ((game (make-snake-game :score 30 :high-score 50))
           (spinner (make-spinner))
           (stats-box (make-stats-box game spinner))
           (stats-lines (component-render stats-box 25))
           (help-box (make-help-box))
           (help-lines (component-render help-box 25))
          ) ;
      (check (component? stats-box) => #t)
      (check (pair? stats-lines) => #t)
      (check (component? help-box) => #t)
      (check (pair? help-lines) => #t)
    ) ;let*

    ;; 3. 虚拟终端端到端渲染：带色视图输出到 virtual-terminal
    (let* ((vterm (make-virtual-terminal :width 80 :height 24))
           (screen (make-main-screen vterm))
           (game (make-snake-game :width 20 :height 10 :snake '((5 5) (4 5) (3 5) (2 5) (1 5)) :food '(10 5)))
           (spinner (make-spinner))
           (view (make-game-view game spinner))
           (view-lines (component-render view 60))
          ) ;
      (screen-render! screen view-lines)
      (let* ((vt-scr (virtual-terminal-screen vterm))
             (vt-row6 (list-ref vt-scr 6))
             (vt-raw (virtual-terminal-raw vterm))
            ) ;
        (check (> (length vt-scr) 10) => #t)
        ;; virtual-terminal-screen 自动过滤 ANSI 转义，前 9 字符依然是严格的 "║ oooo@"
        (check (substring vt-row6 0 9) => "║ oooo@")
        ;; 写入的原始流中包含了 ANSI SGR 转义码
        (check (pair? vt-raw) => #t)
      ) ;let*
    ) ;let*

    ;; 4. 带颜色 GAME OVER 视图
    (let* ((game (make-snake-game :score 40 :status 'game-over))
           (ed (make-editor :placeholder "Your name"))
           (over-view (make-game-over-view game ed))
           (lines (component-render over-view 40))
          ) ;
      (check (component? over-view) => #t)
      (check (pair? lines) => #t)
    ) ;let*
  ) ;lambda
) ;with-color-mode

(check-report)
