;; %editor-data?
;; 判断给定对象是否为编辑器内部数据记录。
;;
;; 语法
;; ----
;; (%editor-data? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为编辑器内部数据记录返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui editor state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-editor-data #f #f #f #t "" 'block #f #f #f #f #f #t #f 0 #f '() #f)))
  (check-true (%editor-data? d))
) ;let

(check-false (%editor-data? "not-data"))
(check-false (%editor-data? 123))

(check-report)
