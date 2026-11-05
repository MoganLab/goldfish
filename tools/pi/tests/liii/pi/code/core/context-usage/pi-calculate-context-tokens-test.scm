;; pi-calculate-context-tokens
;; 由单轮 usage 换算上下文 Token 数（对齐上游 calculateContextTokens：
;; totalTokens 优先，否则 input+output+cacheRead+cacheWrite）
;;
;; 语法
;; ----
;; (pi-calculate-context-tokens usage)
;;
;; 参数
;; ----
;; usage : alist
;; 单轮 assistant usage，键为 input/output/cacheRead/cacheWrite/totalTokens。
;;
;; 返回值
;; ----
;; number
;; 上下文 Token 数；usage 为空时返回 0。

(import (scheme base)
  (liii check)
  (liii pi code core context-usage)
) ;import

(check-set-mode! 'report-failed)

;; 1. totalTokens 优先采用
(check (pi-calculate-context-tokens
         '((input . 100) (output . 50) (cacheRead . 2000) (cacheWrite . 0)
           (totalTokens . 2150))
       ) => 2150)

;; 2. 缺 totalTokens 时按四项求和
(check (pi-calculate-context-tokens
         '((input . 100) (output . 50) (cacheRead . 1900) (cacheWrite . 0))
       ) => 2050)

;; 3. 空 usage 返回 0
(check (pi-calculate-context-tokens '()) => 0)

;; 4. 字符串键 usage 同样识别
(check (pi-calculate-context-tokens '(("input" . 10) ("output" . 5))) => 15)

;; 5. totalTokens 为 0 时退回求和
(check (pi-calculate-context-tokens
         '((input . 10) (output . 5) (totalTokens . 0))
       ) => 15)

(check-report)
