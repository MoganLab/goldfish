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

;; liii 模式会预加载额外的模块，不会抛出异常
(check (catch #t (lambda () (%repl-preload-mode! "liii") #t) (lambda (t i) #f))
  =>
  #t
) ;check

;; default 模式与 liii 模式一致
(check (catch #t (lambda () (%repl-preload-mode! "default") #t) (lambda (t i) #f))
  =>
  #t
) ;check

;; 其它模式不做预加载，同样不会抛出异常
(check (catch #t (lambda () (%repl-preload-mode! "r7rs") #t) (lambda (t i) #f))
  =>
  #t
) ;check

(check-report)
