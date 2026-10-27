(import (scheme base)
  (liii check)
  (liii pi code core compaction fileops))

(check-set-mode! 'report-failed)

;; pi-file-ops?
;; 判断对象是否为文件操作分组 record。
;;
;; 语法
;; ----
;; (pi-file-ops? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean
;; 仅对 pi-file-ops record 返回 #t。
;;
;; 示例
;; ----
;; (pi-file-ops? '()) => #f

(check-true (pi-file-ops? (make-pi-file-ops '() '() '())))
(check-false (pi-file-ops? #f))
(check-false (pi-file-ops? '()))
(check-false (pi-file-ops? (list (cons 'read '()))))

(check-report)
