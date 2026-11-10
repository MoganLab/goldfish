;; make-game-over-view
;; 构造游戏结束视图组件（结算分数与玩家名输入）
;;
;; 语法
;; ----
;; (make-game-over-view game editor-comp)
;;
;; 参数
;; ----
;; game : snake-game?\n结束的游戏状态；editor-comp : component?\n玩家名输入框组件
;;
;; 返回值
;; ------
;; component?\n带 GAME OVER 标语的结算组件

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui editor)
  (liii tui snake logic)
  (liii tui snake view)
  (liii tui snake style)
) ;import

(check-set-mode! 'report-failed)

(with-color-mode #f
  (lambda ()
    (let* ((game (make-snake-game :score 40 :status 'game-over))
           (ed (make-editor :placeholder "Your name"))
           (over-view (make-game-over-view game ed))
          ) ;
      (check (component? over-view) => #t)
      (check (pair? (component-render over-view 40)) => #t)
    ) ;let*
  ) ;lambda
) ;with-color-mode

(with-color-mode #t
  (lambda ()
    (let* ((game (make-snake-game :score 40 :status 'game-over))
           (over-view (make-game-over-view game (make-editor :placeholder "Your name")))
          ) ;
      (check (component? over-view) => #t)
    ) ;let*
  ) ;lambda
) ;with-color-mode

(check-report)
