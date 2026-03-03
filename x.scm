; (define-library (x)
;   (import (scheme base))
;   (begin (display (square 10))))

;; 模拟一个简化版的 expand-import-spec，仅包含 rename 分支

;; 测试用例
(define-syntax test-import
  (lambda (stx)
    (define (test-expand spec-stx ctx)
      (define (transform spec)
        (syntax-case spec (rename)
          ((rename lib (old new) ...)
           #'(display undefined))))
      (transform spec-stx))
    (syntax-case stx ()
      ((_ spec)
       (let ((ctx (syntax _)))
         (with-syntax ((expanded (test-expand (syntax spec) ctx)))
           #'(begin
               (write 'expanded:)
               (write (syntax->datum (syntax expanded)))
               (newline)
               expanded)))))))

;; 调用测试
(test-import (rename (scheme base)))
