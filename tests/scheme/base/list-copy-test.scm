(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; list-copy tests
;; 基本功能测试
(check (list-copy '()) => '())
(check (list-copy '(1 2 3 4 5))
  =>
  '(1 2 3 4 5)
) ;check
(check (list-copy '(a b c d))
  =>
  '(a b c d)
) ;check
(check (list-copy '((1 2) (3 4) (5 6)))
  =>
  '((1 2) (3 4) (5 6))
) ;check
;; 空列表边界条件
(check (list-copy '()) => '())
;; 对象独立性验证 - 确保是浅拷贝
(check-false (eq? (list-copy '(1 2 3)) '(1 2 3))
) ;check-false
;; 突变隔离测试 - 验证列表节点独立性
(let ((orig '(a b c))
      (copy (list-copy '(a b c)))
     ) ;
  (check orig => copy)
  (check-false (eq? orig copy))
  ;; 验证浅拷贝特性
  (let ((mut-copy (list-copy orig)))
    (set-car! mut-copy 'x)
    (check orig => '(a b c))
    (check mut-copy => '(x b c))
  ) ;let
) ;let
(check-report)
(check-report)