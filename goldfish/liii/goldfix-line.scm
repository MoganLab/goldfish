;;; Goldfix Line 模块
;;; 行操作工具函数
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define-library (liii goldfix-line)
  (import (scheme base))
  (import (liii goldfix-comment))
  (import (liii goldfix-line-edit))

  ;; ---------- 导出接口 ----------
  (export line-starts-with-rparen?)
  (export line-ends-in-string-or-comment?)
  (export line-trailing-countable-rparen-count)
  (export line-trailing-unmatched-rparen-count)
  (export line-has-only-trailing-rparens?)
  (export line-is-formatted-right-tag?)
  (export remove-rparens-from-right)
  (export remove-rparens-from-chars)
) ;define-library
