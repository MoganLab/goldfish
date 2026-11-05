;; %repl-banner-lines
;; 返回 REPL 启动横幅的行列表。
;;
;; 语法
;; ----
;; (%repl-banner-lines mode)
;;
;; 参数
;; ----
;; mode : string
;; REPL 模式名称，例如 "liii"、"r7rs"。
;;
;; 返回值
;; ----
;; list
;; 横幅文本的行列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl render)
) ;import

(check-set-mode! 'report-failed)

(let ((lines (%repl-banner-lines "liii")))
  ;; 横幅是非空列表
  (check-true (pair? lines))
  ;; 首行是品牌信息
  (check-true (string-contains (car lines) "Goldfish Scheme"))
  ;; 横幅中包含模式说明与快捷键提示
  (check-true (any (lambda (l) (string-contains l "Mode:")) lines))
  (check-true (any (lambda (l) (string-contains l ",quit")) lines))
  (check-true (any (lambda (l) (string-contains l "F1")) lines))
) ;let

(check-report)
