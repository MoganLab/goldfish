(import (liii check)
  (liii string)
  (liii tui component)
  (liii tui box)
  (liii tui loader)
  (liii tui text-width)
  (liii tui terminal)
  (liii tui main-screen)
  (demo tui snake logic)
  (demo tui snake view)
) ;import

(check-set-mode! 'report-failed)

;; 任务 065: 贪吃蛇中段不可见排查与修复 自动化验收脚本
;; 验证：
;; 1. wrap-text 保留首行前导缩进与空格，不再误删预格式化空格
;; 2. make-game-view 采用组件直通渲染，边框与 padding 参数规范
;; 3. 长蛇与多拐弯/S 形走位各节坐标精确匹配，中段无断裂或丢失
;; 4. 四面墙壁穿透时跨边界两侧身体完整可见
;; 5. 虚拟终端差分渲染多步移动端到端断言

(define (%count-char lines ch)
  (let loop
    ((rem lines) (cnt 0))
    (if (null? rem)
      cnt
      (let* ((str (car rem)) (len (string-length str)))
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

(display "=== 验收点 1: wrap-text 前导空格与缩进保留 ===\n")
(let* ((res1 (wrap-text "  indented line" 20))
       (res2 (wrap-text "    def foo():\n        return 42" 30))
       (res3 (wrap-text "   three spaces" 20))
       (res4 (wrap-text "  hello world foo" 8))
      ) ;
  (check res1 => '("  indented line"))
  (check res2 => '("    def foo():" "        return 42"))
  (check res3 => '("   three spaces"))
  ;; 折行后首行保留 2 格缩进，次行软折行去除行首空格
  (check res4 => '("  hello" "world" "foo"))
) ;let*

(display "=== 验收点 2: 棋盘视图预格式化直通与布局参数规范 ===\n"
) ;display
(let* ((game (make-snake-game
               :width  20
               :height 10
               :snake  '((5 5) (4 5) (3 5) (2 5) (1 5))
               :food   '(10 5)
             ) ;make-snake-game
       ) ;game
       (spinner (make-spinner))
       (view (make-game-view game spinner))
       (lines (component-render view 60))
       (grid (render-grid-lines game))
       (grid-row5 (list-ref grid 5))
       (view-row6 (list-ref lines 6))
      ) ;
  ;; 验证 grid 原始行在 index 0 为空格，index 1..4 为 o，index 5 为 @
  (check (string-ref grid-row5 0) => #\space)
  (check (string-ref grid-row5 1) => #\o)
  (check (string-ref grid-row5 4) => #\o)
  (check (string-ref grid-row5 5) => #\@)
  ;; 验证 view 渲染行包含完整前导空格与蛇身，绝无左移或吞空格
  (check (substring view-row6 0 9) => "║ oooo@")
  ;; 验证食物位置 index 10（对应 view 中 3 字节双线框 + 10 字符 = 第 13 字节）
  (check (string-ref grid-row5 10) => #\*)
) ;let*

(display "=== 验收点 3: 长蛇多拐弯与 S 形走位身体坐标完整可见 ===\n"
) ;display
(let* ((s-coords '((8 2) (8 3) (8 4) (7 4) (6 4) (5 4) (5 5) (5 6) (6 6) (7 6)))
       (len (length s-coords))
       (game (make-snake-game :width 20 :height 10 :snake s-coords :food '(18 8)))
       (spinner (make-spinner))
       (grid (render-grid-lines game))
       (board-comp (make-component (lambda (comp width) grid)))
       (board-box (make-box board-comp :title " SNAKE (liii tui) " :style 'double :padding 0)
       ) ;board-box
       (board-lines (component-render board-box 22))
      ) ;
  ;; 验证 grid 中 @ 恰为 1 个，o 恰为 len - 1 个
  (check (%count-char grid #\@) => 1)
  (check (%count-char grid #\o) => (- len 1))
  ;; 逐节断言蛇头与每一节蛇身坐标
  (let ((head (car s-coords)))
    (check (string-ref (list-ref grid (cadr head)) (car head)) => #\@)
  ) ;let
  (for-each
    (lambda (seg) (check (string-ref (list-ref grid (cadr seg)) (car seg)) => #\o))
    (cdr s-coords)
  ) ;for-each
  ;; 验证 board-lines 边框盒内总数完全吻合
  (check (%count-char board-lines #\@) => 1)
  (check (%count-char board-lines #\o) => (- len 1))
) ;let*

(display "=== 验收点 4: 四面墙壁穿透两侧身体可见性 ===\n")
;; 4.1 水平穿墙：跨越左右边界 (x=0 与 x=19)
(let* ((h-cross '((0 5) (19 5) (18 5) (17 5) (16 5) (15 5)))
       (len (length h-cross))
       (game (make-snake-game :width 20 :height 10 :snake h-cross :food '(10 2)))
       (grid (render-grid-lines game))
      ) ;
  (check (%count-char grid #\@) => 1)
  (check (%count-char grid #\o) => (- len 1))
  (check (string-ref (list-ref grid 5) 0) => #\@)
  (check (string-ref (list-ref grid 5) 19) => #\o)
  (check (string-ref (list-ref grid 5) 18) => #\o)
  (check (string-ref (list-ref grid 5) 1) => #\space)
) ;let*

;; 4.2 垂直穿墙：跨越上下边界 (y=0 与 y=9)
(let* ((v-cross '((7 0) (7 9) (7 8) (7 7) (7 6)))
       (len (length v-cross))
       (game (make-snake-game :width 20 :height 10 :snake v-cross :food '(14 4)))
       (grid (render-grid-lines game))
      ) ;
  (check (%count-char grid #\@) => 1)
  (check (%count-char grid #\o) => (- len 1))
  (check (string-ref (list-ref grid 0) 7) => #\@)
  (check (string-ref (list-ref grid 9) 7) => #\o)
  (check (string-ref (list-ref grid 8) 7) => #\o)
  (check (string-ref (list-ref grid 1) 7) => #\space)
) ;let*

(display "=== 验收点 5: 虚拟终端差分渲染多步移动端到端断言 ===\n"
) ;display
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (screen (make-main-screen vt))
       (spinner (make-spinner))
       ;; 初始向右移动穿墙
       (g0 (make-snake-game
             :width  20
             :height 10
             :snake  '((18 5) (17 5) (16 5) (15 5))
             :dir    'right
             :food   '(2 2)
           ) ;make-snake-game
       ) ;g0
      ) ;
  ;; Tick 1: 蛇头到达 (19 5)
  (let* ((g1 (snake-tick g0)) (v1 (make-game-view g1 spinner)))
    (screen-render! screen (component-render v1 60))
    (let ((row6 (list-ref (screen-lines screen) 6)))
      ;; 棋盘中 18..15 为 o, 19 为 @
      (check (substring row6 0 3) => "║")
    ) ;let
  ) ;let*
  ;; Tick 2: 蛇头穿越至 (0 5), 蛇身在 (19 5), (18 5), (17 5)
  (let* ((g2 (snake-tick (snake-tick g0))) (v2 (make-game-view g2 spinner)))
    (screen-render! screen (component-render v2 60))
    (let ((row6 (list-ref (screen-lines screen) 6)))
      ;; 验证在跨墙时刻，第 6 行在屏幕上左侧为 @, 右侧为 o, 中间为空格
      (check (string-ref row6 3) => #\@)
      (check (string-ref row6 4) => #\space)
      (check (string-ref row6 22) => #\o)
    ) ;let
  ) ;let*
  ;; Tick 3: 蛇头转弯向上 (0 4)
  (let* ((g3
           (snake-tick (snake-turn (snake-tick (snake-tick g0)) 'up))
         ) ;g3
         (v3 (make-game-view g3 spinner))
        ) ;
    (screen-render! screen (component-render v3 60))
    (let ((scr (screen-lines screen)))
      ;; 第 5 行（y=4）的 x=0 处应为 @
      (check (string-ref (list-ref scr 5) 3) => #\@)
      ;; 第 6 行（y=5）的 x=0 处应为 o
      (check (string-ref (list-ref scr 6) 3) => #\o)
    ) ;let
  ) ;let*
) ;let*

(check-report)
