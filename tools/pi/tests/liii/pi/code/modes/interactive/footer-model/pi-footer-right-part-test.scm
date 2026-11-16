;; pi-footer-right-part
;; 组装页脚右下角模型串（含请求进行中切换模型时的“原模型 -> 新模型”双段展示）。
;;
;; 语法
;; ----
;; (pi-footer-right-part provider model thinking-level supports-thinking? active-node pending-from)
;;
;; 参数
;; ----
;; provider、model、thinking-level : (or string #f)
;; supports-thinking? : boolean
;; active-node : (or list #f)
;; pending-from : (or list #f)
;;
;; 返回值
;; ----
;; string

(import (scheme base)
  (liii check)
  (liii pi code modes interactive footer-model)
) ;import

(check-set-mode! 'report)

(check (pi-footer-right-part "google" "gemini-4-hydrogen" "medium" #t #f #f)
  =>
  "(google) gemini-4-hydrogen • medium"
) ;check

(check (pi-footer-right-part "google"
         "gemini-4-hydrogen"
         "medium"
         #t
         #f
         '("minimax-cn" "MiniMax-M3.1-Flash" "medium" #t #f)
       ) ;pi-footer-right-part
  =>
  "(minimax-cn) MiniMax-M3.1-Flash • medium -> (google) gemini-4-hydrogen • medium"
) ;check

;; all-in-one 正在请求且有实际节点时，切换到其他供应商不显示 all-in-one/<chain-id>
(check (pi-footer-right-part "google"
         "gemini-4-hydrogen"
         "medium"
         #t
         #f
         '("all-in-one" "coding-fast" "medium" #t ("minimax-cn" "MiniMax-M3.1-Flash" "medium" 262144 #t))
       ) ;pi-footer-right-part
  =>
  "(minimax-cn) MiniMax-M3.1-Flash • medium -> (google) gemini-4-hydrogen • medium"
) ;check

(check-report)
