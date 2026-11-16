;; pi-footer-set-pending-model!
;; 记录请求进行中正在使用的原模型，并在右下角左侧展示“原模型 -> 新模型”。
;;
;; 语法
;; ----
;; (pi-footer-set-pending-model! comp provider model tl supports? [active-node])
;;
;; 参数
;; ----
;; comp : pi-footer?
;; provider、model、tl : string
;; supports? : boolean
;; active-node : (or list #f)
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
  (let ((line (list-ref (component-render ft 120) 1)))
    (check-true
      (string-contains? line
        "(minimax-cn) MiniMax-M3.1-Flash • medium -> (google) gemini-4-hydrogen • medium"
      ) ;string-contains?
    ) ;check-true
  ) ;let
) ;let

;; all-in-one 原模型收到 active_node 更新时只更新左侧正在请求的节点，不覆盖右侧新模型
(let ((ft (make-pi-footer
            :cwd               "/tmp/test"
            :provider          "all-in-one"
            :model             "coding-fast"
            :thinking-level    "medium"
            :supports-thinking #t
          ) ;make-pi-footer
      ) ;ft
     ) ;
  (pi-footer-set-pending-model! ft "all-in-one" "coding-fast" "medium" #t #f)
  (pi-footer-set-model! ft "google" "gemini-4-hydrogen" #t)
  (pi-footer-set-thinking-level! ft "medium")
  (pi-footer-set-active-node! ft '("minimax-cn" "MiniMax-M3.1-Flash" "high" 262144 #t))
  (check (pi-footer-active-node ft) => #f)
  (let ((line (list-ref (component-render ft 140) 1)))
    (check-true
      (string-contains? line
        "(minimax-cn) MiniMax-M3.1-Flash • high -> (google) gemini-4-hydrogen • medium"
      ) ;string-contains?
    ) ;check-true
    (check-false (string-contains? line "all-in-one"))
  ) ;let
) ;let

(check-report)
