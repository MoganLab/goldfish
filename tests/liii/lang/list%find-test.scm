;; list%find
;; 查找首个满足谓词的元素（对标 Scala List.find）。
;;
;; 语法
;; ----
;; ($ xs :find p)
;;
;; 参数
;; ----
;; p : procedure?
;; 单参谓词；可用 `_` 占位符简写。
;;
;; 返回值
;; ------
;; (option ...)
;; (option 首个满足元素)；不存在返回 (none)（对标 Scala Some/None）。
;;
;; 说明
;; ----
;; :find 产出 option 后可继续接 Option 管线算子：
;; :map / :filter / :for-each / :or-else / :get-or-else / :to-list / :to-vector。
;; 未命中 (none) 时短路传播（None 语义），:get-or-else 回退默认值。

(import (liii check) (liii lang) (liii option))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :find (> _ 1)) => (option 2))
(check ($ '(1 2 3) :find (> _ 9)) => (none))
(check ($ '() :find (> _ 0)) => (none))
(check
  ($ '("a" "bb" "ccc") :find (lambda (s) (> (string-length s) 2)))
  =>
  (option "ccc")
) ;check

(check ($ '(1 2 3) :find (> _ 1) :get-or-else 99) => 2)
(check ($ '(1 2 3) :find (> _ 9) :get-or-else 99) => 99)

;; === :find 命中后接 Option 管线（对标 Scala xs.find(p) 的流式处理）===

;; === 1. :find 命中后接 :map / :get-or-else ===
(check ($ '(1 2 3 4) :find (> _ 2) :map (* _ 10) :get-or-else 0) => 30)

;; === 2. :find 未命中 None 短路传播 ===
(check ($ '(1 2 3 4) :find (> _ 10) :map (* _ 10) :get-or-else 0) => 0)

;; === 3. :find 后接 :filter ===
(check ($ '(1 2 3 4) :find (> _ 1) :filter (even? _) :get-or-else -1) => 2)
(check ($ '(1 2 3 4) :find (> _ 1) :filter (odd? _) :get-or-else -1) => -1)

;; === 4. :find 后接 :for-each 副作用 ===
(let ((acc -1))
  ($ '(1 2 3) :find (> _ 1) :for-each (lambda (x) (set! acc x)))
  (check acc => 2)
) ;let

(let ((acc -1))
  ($ '(1 2 3) :find (> _ 10) :for-each (lambda (x) (set! acc x)))
  (check acc => -1)
) ;let

;; === 5. :find 后接 :or-else 备选兜底 ===
(check ($ '(1 2 3) :find (> _ 10) :or-else (option 99) :get-or-else 0) => 99)

;; === 6. :find 后通过 :to-list / :to-vector 转回集合并继续集合算子链 ===
(check ($ '(1 2 3 4) :find (> _ 2) :to-list) => '(3))
(check ($ '(1 2 3 4) :find (> _ 10) :to-list) => '())
(check ($ '(1 2 3 4) :find (> _ 2) :to-vector) => '#(3))
(check ($ '(1 2 3 4) :find (> _ 10) :to-vector) => '#())
(check ($ '(1 2 3 4) :find (> _ 2) :to-list :map (* _ 10)) => '(30))
(check ($ '(1 2 3 4) :find (> _ 10) :to-list :map (* _ 10)) => '())
(check ($ '(1 2 3 4) :find (> _ 2) :to-vector :map (* _ 10)) => '#(30))

(check-report)
