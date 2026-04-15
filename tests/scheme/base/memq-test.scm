(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; memq 基本功能测试
(check (memq 'a '(a b c)) => '(a b c))
(check (memq 'b '(a b c)) => '(b c))
(check (memq 'c '(a b c)) => '(c))
(check (memq 'd '(a b c)) => #f)
;; 边界值测试
(check (memq '? '()) => #f)
(check (memq 'only '(only)) => '(only))
(check (memq 'single '(single)) => '(single))
(check (memq 'first '(first second third))
  =>
  '(first second third)
) ;check
(check (memq 'last '(first second last)) => '(last))
;; 符号键查找优化测试
(check (memq 'define '(define lambda if))
  =>
  '(define lambda if)
) ;check
(check (memq 'cond '(if else when unless)) => #f)
(check (memq 'car '(cdr cons car)) => '(car))
(check (memq 'procedure '(symbol list number string)) => #f)
;; 重复键边界测试
(check (memq 'repeat '(a b repeat c repeat d))
  =>
  '(repeat c repeat d)
) ;check
(check (memq 'same '(same same same)) => '(same same same))
;; 布尔值边界测试
(check (memq #t '(#t #f #t)) => '(#t #f #t))
(check (memq #f '(#t #f #t)) => '(#f #t))
(check (memq #t '(#f only)) => #f)
;; 字符键测试
(check (memq #\a '(#\b #\a #\c)) => '(#\a #\c))
(check (memq #\x '(#\a #\b #\c)) => #f)
(check (memq #\newline '(a #\newline b)) => '(#\newline b))
;; 数字边界测试
(check (memq 42 '(1 42 3)) => '(42 3))
(check (memq 0 '(0 1 2)) => '(0 1 2))
(check (memq -1 '(0 1 2)) => #f)
(check (memq 100 '(100 200 300)) => '(100 200 300))
;; 特殊符号字符测试
(check (memq 'λ '(λ α β)) => '(λ α β))
(check (memq '$ '(symbol $ delta)) => '($ delta))
;; 过程对象边界测试
(let ((start car) (next cdr) (last cons))
  (check (memq start (list car cdr start))
    =>
    (list car cdr start)
  ) ;check
  (check (memq next (list cons list)) => #f)
) ;let
;; 复合结构测试
(let ((test-list '(cons list append)))
  (check (memq 'list test-list) => '(list append))
  (check (memq 'other-symbol test-list) => #f)
) ;let
;; 点对结构测试
(check (memq 'center '(left center right top))
  =>
  '(center right top)
) ;check
(check (memq 'middle '(begin middle end)) => '(middle end))
(check-report)