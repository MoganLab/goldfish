(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-join
;; 拼接路径。
;;
;; 语法
;; ----
;; (path-join base seg1 seg2 ...) -> path-value
;;
;; 参数
;; ----
;; base : path-value | string?
;; 基础路径。
;; seg1, seg2, ... : string?
;; 要拼接的路径段。
;;
;; 返回值
;; ----
;; path-value
;; 返回拼接后的路径值。
;;
;; 描述
;; ----
;; path-join 是 rich-path 中 :/ 的函数式版本。

(let ((sep (string (os-sep))))
  ;; path-join 测试
  (check (path->string (path-join (path "tmp") "demo.txt"))
         => (string-append "tmp" sep "demo.txt")
  ) ;check
  (check (path->string (path-join (path "tmp") "a" "b" "c.txt"))
         => (string-append "tmp" sep "a" sep "b" sep "c.txt")
  ) ;check

  (when (not (os-windows?))
    (check (path->string (path-join (path-root) "tmp" "demo.txt")) => "/tmp/demo.txt")
    (check-true (path-equals? (path-join (path-root) (path "tmp/demo.txt"))
                              (path "/tmp/demo.txt"))
    ) ;check-true
  ) ;when
) ;let

(check-report)
