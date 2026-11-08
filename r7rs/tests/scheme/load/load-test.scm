;; load
;; 读取文件并按顺序求值其中的每个 datum。
;;
;; 语法
;; ----
;; (load filename)
;; (load filename environment)
;;
;; 参数
;; ----
;; filename : string
;; 文件名字符串。
;;
;; environment : environment 可选
;; 求值环境。省略时使用交互环境（顶层环境）。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 描述
;; ----
;; 1. `load` 把文件内容当作 Scheme 程序，按顺序求值每个顶层表达式。
;; 2. 提供 environment 参数时，文件内容在该环境中求值；
;;    省略时在交互环境（顶层环境）中求值。
;; 3. 文件中定义的绑定在 load 返回后仍然可见。
;;
;; 错误处理
;; --------
;; io-error
;;   当文件不存在或不可读时抛出。
;;
;; (load "path/to/file.scm")
;; (load "path/to/file.scm" (environment '(scheme base)))

(import (liii check) (scheme base) (scheme file) (scheme eval) (scheme load))
(check-set-mode! 'report-failed)

;; 准备被加载的文件：写入 (define loaded-value 42)

(define tmp-file "tests/scheme/load/test-load-target.scm")
(with-output-to-file tmp-file
  (lambda () (write-string "(define loaded-value 42)"))
) ;with-output-to-file

;; 1. 基本行为：顶层环境中定义的绑定在 load 后可见
(check-true (procedure? load))
(load tmp-file)
(check loaded-value => 42)

;; 2. 指定 environment 参数：定义进入该环境

(define env (environment '(scheme base)))
(load tmp-file env)
(check (eval 'loaded-value env) => 42)

;; 3. 文件不存在时抛 file error
(check-catch 'io-error
  (load "tests/scheme/load/nonexistent-load-target-1263.scm")
) ;check-catch

;; 清理
(delete-file tmp-file)

(check-report)
