;; pi-trunc-result-first-line-exceeds-limit?
;; 获取文本截断结果 record 的 first-line-exceeds-limit? 字段。
;;
;; 语法
;; ----
;; (pi-trunc-result-first-line-exceeds-limit? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 文本截断结果 record 实例。
;;
;; 返回值
;; ----
;; boolean?
;; first-line-exceeds-limit? 字段的值。

(import (liii check) (liii pi ai tools types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-trunc-result "line1\nline2" #t "max-lines" 100 2048 2 16 #t #f)
       ) ;obj-a
       (obj-b (%make-pi-trunc-result "完整内容" #f #f 1 8 1 8 #f #f))
      ) ;
  (check (pi-trunc-result-first-line-exceeds-limit? obj-a) => #t)
  (check (pi-trunc-result-first-line-exceeds-limit? obj-b) => #f)
) ;let*

(check-report)
