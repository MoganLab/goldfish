;; %repl-show-out
;; 把求值产生的标准输出整理成输出行。
;;
;; 语法
;; ----
;; (%repl-show-out out app-out!)
;;
;; 参数
;; ----
;; out : string
;; 求值过程中捕获的标准输出。
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

;; 以换行结尾的输出按行拆分
(let ((lines '()))
  (%repl-show-out "hello\n" (lambda (l) (set! lines (append lines l))))
  (check lines => '("hello"))
) ;let

;; 多行输出拆分为多行
(let ((lines '()))
  (%repl-show-out "a\nb\n" (lambda (l) (set! lines (append lines l))))
  (check lines => '("a" "b"))
) ;let

;; 没有结尾换行时追加 ↩ 标记
(let ((lines '()))
  (%repl-show-out "hello" (lambda (l) (set! lines (append lines l))))
  (check lines => '("hello↩"))
) ;let

;; 空输出不会产生任何行
(let ((lines '()))
  (%repl-show-out "" (lambda (l) (set! lines (append lines l))))
  (check lines => '())
) ;let

(check-report)
