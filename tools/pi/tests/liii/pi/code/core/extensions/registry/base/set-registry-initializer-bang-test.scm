;; set-registry-initializer!
;; 注册默认注册表的初始化过程（add-registry-initializer! 的别名）。
;;
;; 语法
;; ----
;; (set-registry-initializer! proc)
;;
;; 参数
;; ----
;; proc : procedure
;; 接收注册表记录的无参或单参过程。
;;
;; 返回值
;; ----
;; 不返回
;; 直接记录初始化过程。

(import (scheme base)
  (liii check)
  (liii pi code core extensions registry base)
  (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let* ((reg (make-extension-registry))
       (saved (default-extension-registry))
       (seen '())
      ) ;
  (set-default-extension-registry! reg)
  (set-registry-initializer! (lambda (r) (set! seen (cons r seen))))
  (check (length seen) => 1)
  (check (car seen) => reg)
  (set-default-extension-registry! saved)
) ;let*

(check-report)
