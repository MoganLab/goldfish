;; 目标库 import 结构违规的 hint 依赖：
;; 第二层检查应报告依赖 hint 的违规，归属本文件路径

(define-library (liii depbaduser)
  (import (liii depbadsig))
  (export ok-call)
  (begin
    (define (ok-call x) (g1 x))
  ) ;begin
) ;define-library
