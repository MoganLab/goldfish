;; %repl-eval
;; 在根环境中求值，并返回 (标准输出 错误输出 是否成功 求值结果)。
;;
;; 语法
;; ----
;; (%repl-eval text)
;;
;; 参数
;; ----
;; text : string
;; 待求值的 Scheme 源码文本。
;;
;; 返回值
;; ----
;; list
;; 四元组 `(out err ok? result)`：out 是标准输出，err 是错误输出，
;; ok? 表示求值是否成功，result 是求值结果。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl eval)
) ;import

(check-set-mode! 'report-failed)

;; 正常求值
(let ((res (%repl-eval "(+ 1 2)")))
  (check (car res) => "")
  (check (cadr res) => "")
  (check (caddr res) => #t)
  (check (cadddr res) => 3)
) ;let

;; 求值结果为 #f 时依然属于成功
(let ((res (%repl-eval "(= 1 2)")))
  (check (caddr res) => #t)
  (check (cadddr res) => #f)
) ;let

;; 求值过程中产生的标准输出会被捕获
(let ((res (%repl-eval "(display \"hello\")")))
  (check (car res) => "hello")
  (check (caddr res) => #t)
) ;let

;; 运行时错误会被捕获，ok? 为假
(let ((res (%repl-eval "(/ 1 0)")))
  (check (caddr res) => #f)
  (check-true (> (string-length (cadr res)) 0))
) ;let

;; 顶层求值结果在根环境中持久化
(%repl-eval "(define *goldrepl-eval-var* 42)")
(let ((res (%repl-eval "*goldrepl-eval-var*")))
  (check (caddr res) => #t)
  (check (cadddr res) => 42)
) ;let

;; 多值求值返回值的列表
(let ((res (%repl-eval "(values 1 2 3)")))
  (check (caddr res) => #t)
  (check (cadddr res) => '(1 2 3))
) ;let

;; 无值求值返回未指定值
(let ((res (%repl-eval "(values)")))
  (check (caddr res) => #t)
  (check (eq? (cadddr res) (if #f #f)) => #t)
) ;let

;; 求值空列表
(let ((res (%repl-eval "'()")))
  (check (caddr res) => #t)
  (check (cadddr res) => '())
) ;let

(check-report)
