(import (scheme base)
  (liii check)
  (liii pi code core compaction tokens))

(check-set-mode! 'report-failed)

;; pi-context-token-estimate?
;; 判断对象是否为上下文 token 估算结果 record。
;;
;; 语法
;; ----
;; (pi-context-token-estimate? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean
;; 仅对 pi-context-token-estimate record 返回 #t。
;;
;; 示例
;; ----
;; (pi-context-token-estimate? '()) => #f

(check-true (pi-context-token-estimate? (make-pi-context-token-estimate 0 0 0)))
(check-false (pi-context-token-estimate? #f))
(check-false (pi-context-token-estimate? '()))
(check-false (pi-context-token-estimate? (list (cons 'tokens 0))))

(check-report)
