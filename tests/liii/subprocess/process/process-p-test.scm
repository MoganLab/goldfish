(import (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; process?
;; 判断给定对象是否为子进程对象。
;;
;; 语法
;; ----
;; (process? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检测的对象。
;;
;; 返回值
;; ----
;; boolean
;; 如果是子进程对象返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (process? 42) => #f
;; (process? "not-a-proc") => #f

;; 非进程对象返回 #f
(check (process? 42) => #f)
(check (process? 3.14) => #f)
(check (process? "not-a-proc") => #f)
(check (process? '()) => #f)
(check (process? '(1 2 3)) => #f)
(check (process? #t) => #f)
(check (process? #f) => #f)
(check (process? 'proc) => #f)
(check (process? #(1 2 3)) => #f)
(check (process? (lambda () 1)) => #f)

;; 合法进程对象返回 #t
(let ((proc (process-spawn '("python3" "-c" "import sys; sys.exit(0)"))))
  (check (process? proc) => #t)
  (process-wait proc 2000)
  (check (process? proc) => #t)
  (process-close proc)
) ;let

;; 参数错误
(check-catch 'wrong-number-of-args (process?))
(check-catch 'wrong-number-of-args (process? 1 2))

(check-report)
