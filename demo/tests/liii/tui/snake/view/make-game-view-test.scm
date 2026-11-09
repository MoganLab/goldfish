;; make-game-view
;; 构造游戏整体视图组件（棋盘 + 侧栏自适应布局）
;;
;; 语法
;; ----
;; (make-game-view game spinner [term-width] [term-height])
;;
;; 参数
;; ----
;; game : snake-game?\n当前游戏状态；spinner : spinner?\n心跳指示器
;;
;; term-width : integer? (可选，默认 60)\n终端列数；term-height : integer? (可选，默认 24)\n终端行数
;;
;; 返回值
;; ------
;; component?\n自适应布局的整体视图组件

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui loader)
  (liii tui snake logic)
  (liii tui snake view)
  (liii tui snake style)
) ;import

(check-set-mode! 'report-failed)

;; 1. 无色模式：宽终端水平布局，视图行与网格对应
(with-color-mode #f
  (lambda ()
    (let* ((game (make-snake-game :width 20 :height 10
                     :snake '((5 5) (4 5) (3 5) (2 5) (1 5)) :food '(10 5)
                   ) ;make-snake-game
             ) ;game
           (view (make-game-view game (make-spinner)))
           (lines (component-render view 60))
          ) ;
      (check (component? view) => #t)
      (check (pair? lines) => #t)
      (check (> (length lines) 5) => #t)
      (check (substring (list-ref lines 6) 0 9) => "║ oooo@")
    ) ;let*
  ) ;lambda
) ;with-color-mode

;; 2. 自适应布局：宽/中/窄终端均可渲染，窄终端纵向行数增加
(with-color-mode #f
  (lambda ()
    (let* ((game (make-snake-game :width 20 :height 10))
           (spinner (make-spinner))
          ) ;
      (check (pair? (component-render (make-game-view game spinner 80 24) 80)) => #t)
      (check (pair? (component-render (make-game-view game spinner 48 20) 48)) => #t)
      (let ((lines-narrow (component-render (make-game-view game spinner 30 20) 30)))
        (check (pair? lines-narrow) => #t)
        (check (>= (length lines-narrow) 18) => #t)
      ) ;let
    ) ;let*
  ) ;lambda
) ;with-color-mode

(check-report)
