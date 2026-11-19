;; border-style-resolve
;; 解析边框风格配置，支持预设符号或自定义边框对象。
;;
;; 语法
;; ----
;; (border-style-resolve style)
;;
;; 参数
;; ----
;; style : symbol? 或 border-style?
;; 边框风格预设符号（'single, 'double, 'rounded, 'ascii, 'none）或已有的 border-style 实例。
;;
;; 返回值
;; ----
;; border-style?
;; 解析得到的边框风格实例。

(import (scheme base) (liii check) (liii tui box border))

(check-set-mode! 'report-failed)

(let ((b (border-style-resolve 'single)))
  (check-true (border-style? b))
  (check (border-style-name b) => 'single)
  (check (border-style-tl b) => "┌")
  (check (border-style-tr b) => "┐")
) ;let

(let ((b (border-style-resolve 'ascii)))
  (check (border-style-tl b) => "+")
  (check (border-style-h b) => "-")
) ;let

(let ((custom (make-border-style "+" "+" "+" "+" "-" "|")))
  (check (border-style-resolve custom) => custom)
) ;let

(check-catch 'value-error (border-style-resolve 'unknown))
(check-catch 'type-error (border-style-resolve 123))

(check-report)
