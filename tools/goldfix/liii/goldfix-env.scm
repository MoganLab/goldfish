;;; Goldfix Env 模块
;;; Environment 对外门面
;;;
;;; Copyright (c) 2024 Liii Network
;;; All Rights Reserved

(define-library (liii goldfix-env)
  (import (scheme base))
  (import (liii goldfix-scheme))
  (import (liii goldfix-env-tag))
  (import (liii goldfix-env-core))
  (import (liii goldfix-env-scan))

  ;; ---------- 导出接口 ----------
  ;; Environment 记录类型
  (export make-env)
  (export env?)
  (export env-tag)
  (export env-lparen-line)
  (export env-lparen-col)
  (export env-parent)
  (export env-children)
  (export env-set-children!)
  (export env-rparen-line)
  (export env-set-rparen-line!)

  ;; 扫描环境相关函数
  (export scan-environments)
  (export scan-environment-details)
  (export scan-claimed-rparen-lines)
  (export env-detail?)
  (export env-detail-env)
  (export env-detail-close-line)
  (export env-detail-explicit-rparen-line)
  (export char-identifier?)
  (export extract-tag)
  (export extract-right-tag)

  ;; 右标记行生成函数
  (export make-right-tag-line)
) ;define-library
