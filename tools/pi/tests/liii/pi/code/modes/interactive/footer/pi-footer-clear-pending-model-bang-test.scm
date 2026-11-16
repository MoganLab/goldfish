;; pi-footer-clear-pending-model!
;; 清除请求进行中的原模型快照，右下角恢复仅显示当前模型。
;;
;; 语法
;; ----
;; (pi-footer-clear-pending-model! comp)
;;
;; 参数
;; ----
;; comp : pi-footer?
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui component)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report)

(let ((ft (make-pi-footer
            :cwd               "/tmp/test"
            :provider          "minimax-cn"
            :model             "MiniMax-M3.1-Flash"
            :thinking-level    "medium"
            :supports-thinking #t
          ) ;make-pi-footer
      ) ;ft
     ) ;
  (pi-footer-set-pending-model! ft "minimax-cn" "MiniMax-M3.1-Flash" "medium" #t)
  (pi-footer-set-model! ft "google" "gemini-4-hydrogen" #t)
  (pi-footer-set-thinking-level! ft "medium")
  (pi-footer-clear-pending-model! ft)
  (let ((line (list-ref (component-render ft 120) 1)))
    (check-true (string-contains? line "(google) gemini-4-hydrogen • medium"))
    (check-false (string-contains? line "->"))
  ) ;let
) ;let

(check-report)
