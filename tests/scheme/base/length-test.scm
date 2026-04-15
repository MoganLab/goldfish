(import (liii check))
(import (liii list))
(import (scheme base))
(check-set-mode! 'report-failed)
;; length 基本测试：空列表和非空列表
(check (length '()) => 0)
(check (length '(a)) => 1)
(check (length '(a b)) => 2)
(check (length '(a b c)) => 3)
(check (length '(1 2 3 4 5)) => 5)
;; length 嵌套列表测试
(check (length '((a) b c)) => 3)
(check (length '((a b) (c d) e)) => 3)
(check (length '((a) (b) (c))) => 3)
(check (length '(((a b) c) d e)) => 3)
(check (length '((1 2 3 4) (5 6 7 8))) => 2)
;; length 复杂数据结构测试
(check (length '((first 1) (second 2) (third 3))) => 3)
(check (length '("hello" "world" "test")) => 3)
(check (length '(#t #f 'symbol)) => 3)
(check (length '(42 3.14 "string" #t)) => 4)
;; length 边界测试：各种规模列表
(check (length '(a)) => 1)
(check (length '(a b)) => 2)
(check (length '(a b c d e f g h i j)) => 10)
(check (length '(long list with many elements potentially spanning multiple lines)
       ) ;length
  =>
  9
) ;check
;; length 空列表和单元素列表测试
(check (length '()) => 0)
(check (length (list)) => 0)
(check (length (cons 'a '())) => 1)
(check (length '(single)) => 1)
;; length 字符和数字列表测试
(check (length '(#\a #\b #\c #\d)) => 4)
(check (length '(0 1 2 3 4 5 6 7 8 9)) => 10)
(check (length '("zero" "one" "two" "three" "four")) => 5)
;; length 列表构造函数测试
(check (length (make-list 3 #\a)) => 3)
(check (length (make-list 0)) => 0)
(check (length (make-list 5 'value)) => 5)
;; length 列表操作函数结果测试
(check (length (append '(1 2) '(3 4 5))) => 5)
(check (length (append '() '(a b c))) => 3)
(check (length (append '(x y) '())) => 2)
(check (length (reverse '(a b c d))) => 4)
(check (length (reverse '())) => 0)
;; length 特殊测试：字符串和向量等
(check (length "string") => 6)
(check (length '#(1 2 3)) => 3)
(check (length 123) => #f)
(check (length 3.14) => #f)
(check (length #\a) => #f)
(check (length 'symbol) => #f)
;; length 点对结构（improper lists）
(check (length '(a . b)) => -1)
(check (length (cons 1 2)) => -1)
(check (length (cons 'a 'b)) => -1)
(check (length '(a b . c)) => -2)
(check (length '(x (a) . y)) => -2)
(check (length '(a b c . d)) => -3)
;; length 特殊边界测试
(check (length '(())) => 1)
(check (length '(() () ())) => 3)
(check (length '(() a b 3 c)) => 5)
;; length Unicode字符串列表测试
(check (length '("中国" "美国" "日本")) => 3)
(check (length '("hello" "世界" "123")) => 3)
;; length 程序构造测试
(check (length (let ((lst '(a b c))) lst)) => 3)
(check (length (map square '(1 2 3 4))) => 4)
(check (length (filter symbol? '(a 1 b 2 c 3))) => 3)
;; length URL列表测试
(check (length '("http://example.com" "https://test.org"))
  =>
  2
) ;check
(check (length '(user admin guest moderator)) => 4)
(check-report)