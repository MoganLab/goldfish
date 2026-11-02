;; border-render-line
;; 为一行文本内容添加左右竖线边框与内边距。
;;
;; 语法
;; ----
;; (border-render-line border content-line content-width pad-left pad-right)
;;
;; 参数
;; ----
;; border : border-style?
;; 边框风格实例。
;;
;; content-line : string?
;; 内部内容文本单行。
;;
;; content-width : integer?
;; 内部内容区可用宽度。
;;
;; pad-left : integer?
;; 左侧内边距空格数。
;;
;; pad-right : integer?
;; 右侧内边距空格数。
;;
;; 返回值
;; ----
;; string?
;; 加上两侧边框与内边距后的文本行。

(import (scheme base) (liii check) (liii tui text-width) (liii tui box border))

(check-set-mode! 'report-failed)

(let ((single (border-style-resolve 'single)) (none (border-style-resolve 'none)))
  (let ((line (border-render-line single "hi" 6 1 1)))
    (check (display-width line) => 10)
    (check line => "│ hi     │")
  ) ;let
  (let ((line-cjk (border-render-line single "你好" 6 1 1)))
    (check (display-width line-cjk) => 10)
    (check line-cjk => "│ 你好   │")
  ) ;let
  (let ((line-none (border-render-line none "hi" 6 1 1)))
    (check (display-width line-none) => 8)
    (check line-none => " hi     ")
  ) ;let
) ;let

(check-report)
