;; %repl-preload-mode!
;; 按 REPL 模式预加载额外的模块。
;;
;; 语法
;; ----
;; (%repl-preload-mode! mode)
;;
;; 参数
;; ----
;; mode : string
;; REPL 模式名称，例如 "liii"、"r7rs"。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未定义，函数通过预加载模块生效。

(import (scheme base) (liii check) (liii string) (liii goldrepl loop))

(check-set-mode! 'report-failed)

;; 判断符号是否已经绑定到根环境中

(define (rootlet-has? name)
  (catch #t
    (lambda ()
      (eval-string (string-append "(if (procedure? " name ") #t #f)") (rootlet))
    ) ;lambda
    (lambda (t i) #f)
  ) ;catch
) ;define

;; liii 模式会把 (liii error) 等模块导入根环境
(check (rootlet-has? "type-error?") => #f)

(%repl-preload-mode! "liii")

;; 预加载之后，(liii error) 的导出在根环境中可见
(check (rootlet-has? "type-error?") => #t)

;; default 模式与 liii 模式一致，同样会预加载模块
(check (rootlet-has? "type-error?") => #t)

;; 其它模式不做预加载，不会抛出异常
(check (catch #t (lambda () (%repl-preload-mode! "r7rs") #t) (lambda (t i) #f))
  =>
  #t
) ;check

(check-report)
