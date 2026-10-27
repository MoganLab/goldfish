;; 类型收窄测试用的原库：
;; my-assoc 返回 (or #f pair?)，my-cdr 需要 pair?。
;; 收窄正确时，有守卫的调用不报；无守卫的调用仍要报。

(define-library (liii narrow)
  (import (liii reexport-dep))
  (export guarded-if guarded-if2 guarded-and guarded-and2 guarded-cond unguarded)
  (begin

    ;; if 的 test 为真时，(or #f pair?) 收窄为 pair?
    (define (guarded-if alist)
      (let ((v (my-assoc 'a alist)))
        (if v (my-cdr v) #f)
      ) ;let
    ) ;define

    ;; if 的 test 是 and 形式时，按各合取项收窄
    (define (guarded-if2 alist)
      (let ((v (my-assoc 'a alist)))
        (if (and v (pair? v)) (my-cdr v) #f)
      ) ;let
    ) ;define

    ;; and 左侧为真时收窄
    (define (guarded-and alist)
      (let ((v (my-assoc 'a alist)))
        (and v (my-cdr v))
      ) ;let
    ) ;define

    ;; and 左侧为真时收窄，且后续嵌套调用同样生效
    (define (guarded-and2 alist)
      (let ((v (my-assoc 'a alist)))
        (and v (my-cdr (my-cons v 1)))
      ) ;let
    ) ;define

    ;; cond 分支按其测试为真收窄
    (define (guarded-cond alist)
      (let ((v (my-assoc 'a alist)))
        (cond ((pair? v) (my-cdr v))
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; 无守卫：确定的类型冲突仍要报告
    (define (unguarded alist)
      (let ((v (my-assoc 'a alist)))
        (my-cdr v)
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
