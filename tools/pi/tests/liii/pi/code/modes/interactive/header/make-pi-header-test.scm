;; make-pi-header
;; 构造并返回新的 make-pi-header 实例组件。
;;
;; 语法
;; ----
;; (make-pi-header ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; component?
;; 返回构造的实例。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui text-width)
  (liii tui component)
  (liii pi code modes interactive header)
) ;import

(check-set-mode! 'report)

(let* ((hdr (make-pi-header))
       (lines (component-render hdr 80))
       (plain-lines (map strip-ansi lines))
      ) ;let*
  (check (component? hdr) => #t)
  (check (length lines) => 6)
  (check (list-ref plain-lines 0) => "")
  (check-true (string-contains? (list-ref plain-lines 1) (string-append " gf v" (version))))
  (check-true (string-contains? (list-ref plain-lines 2) "escape interrupt"))
  (check-true (string-contains? (list-ref plain-lines 2) "ctrl+c/ctrl+d clear/exit"))
  (check-true (string-contains? (list-ref plain-lines 2) "ctrl+z undo"))
  (check-true (string-contains? (list-ref plain-lines 2) "/ commands"))
  (check-true (string-contains? (list-ref plain-lines 2) "! bash"))
  (check-false (string-contains? (list-ref plain-lines 2) "ctrl+o"))
  (check (list-ref plain-lines 3) => "")
  (check-true (string-contains? (list-ref plain-lines 4) "Goldfish Scheme Agent can explain its own features"))
  (check (list-ref plain-lines 5) => "")
) ;let*

(let* ((hdr (make-pi-header :version "1.2.3"))
       (lines (component-render hdr 80))
       (plain-lines (map strip-ansi lines))
      ) ;let*
  (check-true (string-contains? (list-ref plain-lines 1) " gf v1.2.3"))
) ;let*

(check-report)
