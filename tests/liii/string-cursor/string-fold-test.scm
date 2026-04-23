(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-fold: 从左到右折叠
(check (string-fold (lambda (c acc) (cons c acc)) '() "abc") => '(#\c #\b #\a))
(check (string-fold (lambda (c count) (+ count 1)) 0 "abc") => 3)
(check (string-fold (lambda (c count) (+ count 1)) 0 "") => 0)
(check (string-fold (lambda (c count) (+ count 1)) 0 "中文") => 2)
(check (string-fold (lambda (c count) (+ count 1)) 0 "🎉🎊") => 2)

;; string-fold-right: 从右到左折叠
(check (string-fold-right cons '() "abc") => '(#\a #\b #\c))
(check (string-fold-right (lambda (c acc) (string-append (string c) acc)) "" "abc") => "abc")
(check (string-fold-right (lambda (c count) (+ count 1)) 0 "中文") => 2)

;; string-for-each-cursor: 对每个cursor应用函数
(let ((result '()))
  (string-for-each-cursor
    (lambda (cur) (set! result (cons cur result)))
    "abc")
  (check (map (lambda (c) (string-ref/cursor "abc" c)) (reverse result)) => '(#\a #\b #\c)))

;; string-count: 统计满足条件的字符数
(check (string-count char-alphabetic? "abc123") => 3)
(check (string-count char-numeric? "abc123") => 3)
(check (string-count char-alphabetic? "中文") => 2)
(check (string-count (lambda (c) #t) "") => 0)

(check-report)
