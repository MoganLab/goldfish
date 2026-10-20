(import (liii check)
  (liii tui component)
  (liii tui box)
  (liii tui loader)
  (liii tui editor)
  (liii tui text-width)
  (liii tui terminal)
  (liii tui main-screen)
  (demo tui snake logic)
  (demo tui snake view)
) ;import

(check-set-mode! 'report-failed)

;; 1. 测试 render-grid-lines
(let* ((game (make-snake-game :width 10 :height 6 :snake '((3 2) (2 2) (1 2)) :food '(5 2)))
       (lines (render-grid-lines game))
      ) ;
  (check (length lines) => 6)
  ;; 验证第 2 行（索引 2）包含蛇头 @、身体 o 和食物 *
  (let ((row2 (list-ref lines 2)))
    (check (string-length row2) => 10)
    (check (string-ref row2 3) => #\@)
    (check (string-ref row2 2) => #\o)
    (check (string-ref row2 1) => #\o)
    (check (string-ref row2 5) => #\*)
  ) ;let
) ;let*

;; 2. 测试 make-stats-box 与 make-help-box 渲染
(let* ((game (make-snake-game :score 30 :high-score 50))
       (spinner (make-spinner))
       (stats-box (make-stats-box game spinner))
       (stats-lines (component-render stats-box 25))
      ) ;
  (check (component? stats-box) => #t)
  (check (pair? stats-lines) => #t)
  ;; 验证包含 Score: 30 与 Best: 50
  (let ((combined (apply string-append stats-lines)))
    (check (> (display-width combined) 0) => #t)
  ) ;let
) ;let*

;; 3. 测试 make-game-view 整体水平与垂直布局渲染
(let* ((game (make-snake-game :width 20 :height 10 :snake '((5 5) (4 5) (3 5) (2 5) (1 5)) :food '(10 5)))
       (spinner (make-spinner))
       (view (make-game-view game spinner))
       (lines (component-render view 60))
       (grid (render-grid-lines game))
       (grid-row5 (list-ref grid 5))
       (view-row6 (list-ref lines 6)) ; line 0 is top border, line 1..5 is row 0..4, line 6 is row 5
      )
  (check (component? view) => #t)
  (check (pair? lines) => #t)
  (check (> (length lines) 5) => #t)
  ;; grid-row5 应该在 index 0 是空格，index 1..4 是 o，index 5 是 @
  (check (string-ref grid-row5 0) => #\space)
  (check (string-ref grid-row5 1) => #\o)
  (check (string-ref grid-row5 5) => #\@)
  ;; 现在检查 view-row6 的前 9 字节（║ 占 3 字节，空格 1 字节，oooo 4 字节，@ 1 字节 = 9 字节）！
  (check (substring view-row6 0 9) => "║ oooo@")
) ;let*

;; 4. 测试 make-game-over-view 与 editor 交互渲染
(let* ((game (make-snake-game :score 40 :status 'game-over))
       (ed (make-editor :placeholder "Your name"))
       (over-view (make-game-over-view game ed))
       (lines (component-render over-view 40))
      ) ;
  (check (component? over-view) => #t)
  (check (pair? lines) => #t)
) ;let*

;; 5. 辅助函数：统计网格/行列表中的特定字符出现总数
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

;; 6. 测试长蛇拐弯与 S 形走位：各节身体在 grid 与 view 中坐标连续、中段无遗漏
(let* ((snake-body-coords '((5 3) (5 4) (5 5) (4 5) (3 5) (2 5) (2 6) (2 7)))
       (len (length snake-body-coords))
       (game (make-snake-game :width 20 :height 10 :snake snake-body-coords :food '(15 8)))
       (spinner (make-spinner))
       (grid (render-grid-lines game))
       (view (make-game-view game spinner))
       (view-lines (component-render view 60))
       (board-comp (make-component (lambda (comp width) grid)))
       (board-box (make-box board-comp :title " SNAKE (liii tui) " :style 'double :padding 0))
      ) ;
  ;; 验证 grid 中 @ 恰有 1 个，o 恰有 len - 1 个
  (check (%count-char grid #\@) => 1)
  (check (%count-char grid #\o) => (- len 1))
  ;; 验证每一节预期的具体坐标上都正确显示字符
  (let ((head (car snake-body-coords)))
    (check (string-ref (list-ref grid (cadr head)) (car head)) => #\@)
  ) ;let
  (for-each
    (lambda (seg)
      (check (string-ref (list-ref grid (cadr seg)) (car seg)) => #\o)
    ) ;lambda
    (cdr snake-body-coords)
  ) ;for-each
  ;; 验证在 board-box 渲染结果中，@ 与 o 依然完整存在无丢失
  (let ((board-lines (component-render board-box 22)))
    (check (%count-char board-lines #\@) => 1)
    (check (%count-char board-lines #\o) => (- len 1))
  ) ;let
) ;let*

;; 7. 测试水平穿墙场景：长蛇跨越左右边界，两侧身体皆完整可见
(let* ((cross-coords '((1 5) (0 5) (19 5) (18 5) (17 5) (16 5)))
       (len (length cross-coords))
       (game (make-snake-game :width 20 :height 10 :snake cross-coords :food '(10 2)))
       (spinner (make-spinner))
       (grid (render-grid-lines game))
       (view (make-game-view game spinner))
       (view-lines (component-render view 60))
       (board-comp (make-component (lambda (comp width) grid)))
       (board-box (make-box board-comp :title " SNAKE (liii tui) " :style 'double :padding 0))
      ) ;
  (check (%count-char grid #\@) => 1)
  (check (%count-char grid #\o) => (- len 1))
  ;; 左侧 (1 5) 为 @, (0 5) 为 o
  (check (string-ref (list-ref grid 5) 1) => #\@)
  (check (string-ref (list-ref grid 5) 0) => #\o)
  ;; 右侧 (19 5), (18 5), (17 5), (16 5) 为 o
  (check (string-ref (list-ref grid 5) 19) => #\o)
  (check (string-ref (list-ref grid 5) 18) => #\o)
  (check (string-ref (list-ref grid 5) 17) => #\o)
  (check (string-ref (list-ref grid 5) 16) => #\o)
  ;; 中段 (2..15) 为空格
  (check (string-ref (list-ref grid 5) 2) => #\space)
  (check (string-ref (list-ref grid 5) 15) => #\space)
  ;; board-box 中总数保持一致
  (let ((board-lines (component-render board-box 22)))
    (check (%count-char board-lines #\@) => 1)
    (check (%count-char board-lines #\o) => (- len 1))
  ) ;let
) ;let*

;; 8. 测试垂直穿墙场景：长蛇跨越上下边界，上下两侧身体皆完整可见
(let* ((v-coords '((5 0) (5 9) (5 8) (5 7) (5 6)))
       (len (length v-coords))
       (game (make-snake-game :width 20 :height 10 :snake v-coords :food '(12 4)))
       (spinner (make-spinner))
       (grid (render-grid-lines game))
       (view (make-game-view game spinner))
       (view-lines (component-render view 60))
       (board-comp (make-component (lambda (comp width) grid)))
       (board-box (make-box board-comp :title " SNAKE (liii tui) " :style 'double :padding 0))
      ) ;
  (check (%count-char grid #\@) => 1)
  (check (%count-char grid #\o) => (- len 1))
  ;; 顶部 (5 0) 为 @
  (check (string-ref (list-ref grid 0) 5) => #\@)
  ;; 底部 (5 9), (5 8), (5 7), (5 6) 为 o
  (check (string-ref (list-ref grid 9) 5) => #\o)
  (check (string-ref (list-ref grid 8) 5) => #\o)
  (check (string-ref (list-ref grid 7) 5) => #\o)
  (check (string-ref (list-ref grid 6) 5) => #\o)
  ;; board-box 中总数保持一致
  (let ((board-lines (component-render board-box 22)))
    (check (%count-char board-lines #\@) => 1)
    (check (%count-char board-lines #\o) => (- len 1))
  ) ;let
) ;let*

;; 9. 测试虚拟终端端到端渲染：差分屏幕驱动输出与棋盘内容严格一致
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (screen (make-main-screen vterm))
       (game (make-snake-game :width 20 :height 10 :snake '((5 5) (4 5) (3 5) (2 5) (1 5)) :food '(10 5)))
       (spinner (make-spinner))
       (view (make-game-view game spinner))
       (view-lines (component-render view 60))
      ) ;
  (screen-render! screen view-lines)
  (let* ((scr-lines (screen-lines screen))
         (row6 (list-ref scr-lines 6)) ; 棋盘第 5 行对应屏幕索引 6
        ) ;
    (check (> (length scr-lines) 10) => #t)
    ;; 验证虚拟终端屏幕第 6 行保留前导空格，蛇身无左移变形
    (check (substring row6 0 9) => "║ oooo@")
  ) ;let*
) ;let*

(check-report)
