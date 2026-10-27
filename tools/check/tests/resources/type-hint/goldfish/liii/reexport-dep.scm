;; 只再导出依赖过程，不提供 hint；检查器应穿透到 (liii dep)。
(define-library (liii reexport-dep)
  (import (liii dep))
  (export my-assoc my-cdr my-cons)
)
