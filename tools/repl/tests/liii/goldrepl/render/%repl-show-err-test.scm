;; %repl-show-err
;; 把求值产生的错误输出整理成输出行。
;;
;; 语法
;; ----
;; (%repl-show-err err app-out!)
;;
;; 参数
;; ----
;; err : string
;; 求值过程中捕获的错误输出。
;; app-out! : procedure
;; 输出一行文本的回调，接收文本行列表。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未定义，函数通过回调输出文本行。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl render)
) ;import

(check-set-mode! 'report-failed)

;; 错误输出按行拆分，并使用错误样式
(let ((lines '()))
  (%repl-show-err "boom" (lambda (l) (set! lines (append lines l))))
  (check (length lines) => 1)
  (check-true (string-contains (car lines) "boom"))
  (check-true (string-contains (car lines) "\x1b;[31m"))
) ;let

;; 多行错误输出
(let ((lines '()))
  (%repl-show-err "a\nb" (lambda (l) (set! lines (append lines l))))
  (check (length lines) => 2)
  (check-true (string-contains (car lines) "a"))
  (check-true (string-contains (cadr lines) "b"))
) ;let

;; 空错误输出不会产生任何行
(let ((lines '()))
  (%repl-show-err "" (lambda (l) (set! lines (append lines l))))
  (check lines => '())
) ;let

(check-report)
