;; service.scm: 业务服务层，依赖 engine.scm
(load "demo/stacktrace/engine.scm")

(define (format-score user score base)
  (cons user (compute-ratio score base))
) ;define
