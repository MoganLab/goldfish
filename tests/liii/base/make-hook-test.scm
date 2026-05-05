(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; make-hook
;; 创建一个新的 hook（过程对象），可用于注册和触发回调函数。
;;
;; 语法
;; ----
;; (make-hook)
;; (make-hook 'arg1 ...)
;;
;; 参数
;; ----
;; arg1 ... : symbol?
;; 可选的参数名列表。参数通过 (hook 'arg) 在回调中获取。
;;
;; 返回值
;; ------
;; procedure?
;; 返回一个 hook 过程对象。
;;
;; 说明
;; ----
;; make-hook 是 S7 内置函数，不在 R7RS 标准中定义。
;; 通过 hook-functions 可以获取或设置 hook 的回调函数列表。
;; 触发 hook 时，回调函数按列表顺序依次执行。
;;
;; 示例
;; ----
;; (define h (make-hook 'code))
;; (set! (hook-functions h) (list (lambda (hook) (hook 'code))))
;; (h 42)  ; 触发回调，code 为 42


;; make-hook 返回一个过程
(check (procedure? (make-hook)) => #t)
(check (procedure? (make-hook 'x)) => #t)
(check (procedure? (make-hook 'a 'b)) => #t)


;; hook-functions 默认值是空列表
(check (hook-functions (make-hook)) => '())


;; 设置 hook-functions 并触发回调
(let ((h (make-hook 'code)) (result #f))
  (set! (hook-functions h) (list (lambda (hook) (set! result (hook 'code)))))
  (h 42)
  (check result => 42)
) ;let


;; 多个回调按顺序执行
(let ((h (make-hook 'x)) (results '()))
  (set! (hook-functions h)
    (list (lambda (hook) (set! results (cons (hook 'x) results)))
      (lambda (hook) (set! results (cons (* 2 (hook 'x)) results)))
    ) ;list
  ) ;set!
  (h 5)
  ;; for-each 按列表顺序执行，所以先第一个回调，再第二个回调
  (check results => '(10 5))
) ;let


;; 无参数的 hook
(let ((h (make-hook)) (called #f))
  (set! (hook-functions h) (list (lambda (hook) (set! called #t))))
  (h)
  (check called => #t)
) ;let


;; 多参数 hook
(let ((h (make-hook 'a 'b)) (result '()))
  (set! (hook-functions h)
    (list (lambda (hook) (set! result (list (hook 'a) (hook 'b)))))
  ) ;set!
  (h 1 2)
  (check result => '(1 2))
) ;let


(check-report)
