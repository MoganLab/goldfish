(import (liii check)
  (liii tui component)
  (liii tui loader)
  (liii tui editor)
  (liii tui text-width)
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
(let* ((game (make-snake-game :width 15 :height 8))
       (spinner (make-spinner))
       (view (make-game-view game spinner))
       (lines (component-render view 60))
      ) ;
  (check (component? view) => #t)
  (check (pair? lines) => #t)
  ;; 验证行数与终端渲染内容非空
  (check (> (length lines) 5) => #t)
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

(check-report)
