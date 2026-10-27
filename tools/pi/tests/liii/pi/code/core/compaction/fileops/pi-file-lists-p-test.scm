(import (scheme base)
  (liii check)
  (liii pi code core compaction fileops))

(check-set-mode! 'report-failed)

;; pi-file-lists?
;; 判断对象是否为压缩摘要文件列表 record。
;;
;; 语法
;; ----
;; (pi-file-lists? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean
;; 仅对 pi-file-lists record 返回 #t。
;;
;; 示例
;; ----
;; (pi-file-lists? '()) => #f

(check-true (pi-file-lists? (make-pi-file-lists '() '())))
(check-false (pi-file-lists? #f))
(check-false (pi-file-lists? '()))
(check-false (pi-file-lists? (list (cons 'read-files '()))))

(check-report)
