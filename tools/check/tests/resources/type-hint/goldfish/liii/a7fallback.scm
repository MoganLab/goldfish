;; A7 fixture: fb-fn 再导出自 (liii a7fallbackdep)，实现库没有 hint 库。
(define-library (liii a7fallback)
  (import (scheme base) (liii a7fallbackdep))
  (export fb-fn)
) ;define-library
