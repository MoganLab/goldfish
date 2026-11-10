;; border-render-bottom
;; 渲染容器组件底边框行。
;;
;; 语法
;; ----
;; (border-render-bottom style width)
;;
;; 参数
;; ----
;; style : border-style?
;; 边框风格实例。
;;
;; width : integer?
;; 边框总宽度。
;;
;; 返回值
;; ----
;; string?
;; 渲染出的底边框单行文本。

(import (scheme base) (liii check) (liii tui box border))

(check-set-mode! 'report-failed)

(let ((b (border-style-resolve 'ascii)))
  (check (border-render-bottom b 6) => "+----+")
) ;let

(let ((b (border-style-resolve 'single)))
  (check (border-render-bottom b 5) => "└───┘")
) ;let

(check-catch 'type-error (border-render-bottom "not-a-style" 6))

(check-report)
