;; get-source-local-path
;; 展开本地来源路径：~ 展开、相对路径基于 cwd，返回归一化绝对路径。
;;
;; 语法
;; ----
;; (get-source-local-path arg)
;;
;; 参数
;; ----
;; arg : string?
;; 本地路径形式的来源。
;;
;; 返回值
;; ----
;; string
;; 归一化后的绝对路径；非路径来源抛 type-error。

(import (liii check) (liii os) (liii path) (liii gfproject get source))

(check-set-mode! 'report-failed)

;; 绝对路径原样归一化
(let ((p (path->string (path-temp-dir))))
  (check (get-source-local-path p) => (path->string (path-resolve (path p) #f)))
) ;let

;; 相对路径基于 cwd
(check (get-source-local-path "./x")
  => (path->string (path-resolve (path-join (getcwd) "x") #f))
) ;check

;; ~ 展开到 home
(check (get-source-local-path "~/x")
  => (path->string (path-resolve (path-join (path-home) "x") #f))
) ;check

;; 非路径来源拒绝
(check-catch 'type-error (get-source-local-path "pi@liii"))
(check-catch 'type-error (get-source-local-path "https://example.com/r.git"))

(check-report)
