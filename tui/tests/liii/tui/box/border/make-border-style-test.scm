;; make-border-style
;; 创建自定义边框风格实例。
;;
;; 语法
;; ----
;; (make-border-style tl tr bl br h v)
;; (make-border-style name tl tr bl br h v)
;;
;; 参数
;; ----
;; name : symbol? (可选，默认为 'custom)
;; 边框样式名称标识。
;;
;; tl : string?
;; 左上角字符。
;;
;; tr : string?
;; 右上角字符。
;;
;; bl : string?
;; 左下角字符。
;;
;; br : string?
;; 右下角字符。
;;
;; h : string?
;; 水平横线字符。
;;
;; v : string?
;; 垂直坚线字符。
;;
;; 返回值
;; ----
;; border-style?
;; 边框风格实例。

(import (scheme base) (liii check) (liii tui box border))

(check-set-mode! 'report-failed)

(let ((custom (make-border-style "1" "2" "3" "4" "=" "!")))
  (check-true (border-style? custom))
  (check (border-style-name custom) => 'custom)
  (check (border-style-tl custom) => "1")
  (check (border-style-tr custom) => "2")
  (check (border-style-bl custom) => "3")
  (check (border-style-br custom) => "4")
  (check (border-style-h custom) => "=")
  (check (border-style-v custom) => "!")
) ;let

(let ((named (make-border-style 'my-style "+" "+" "+" "+" "-" "|")))
  (check (border-style-name named) => 'my-style)
) ;let

(check-catch 'type-error (make-border-style 1 2 3))

(check-report)
