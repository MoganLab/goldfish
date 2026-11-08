;; option%map
;; $ 对 (liii option) 的链式支持（对标 Scala Option.map 及链式操作）。
;;
;; 语法
;; ----
;; ($ opt :map f)
;;
;; 参数
;; ----
;; f : procedure?
;; 单参变换函数；可用 `_` 占位符简写。
;;
;; 返回值
;; ------
;; (option ...)
;; Some 时返回 (option (f v))；None 时短路返回 (none)。
;;
;; 描述
;; ----
;; None 传播：map/filter/flat-map 遇 (none) 短路返回 (none)，
;; 不求值谓词（对标 Scala Option 的 None 短路语义）。
;; :get-or-else 为终结算子，空回退默认值。
;; 本文件同时覆盖 :filter / :flat-map / :get-or-else / :or-else /
;; :exists / :forall / :contains 等 Option 管线算子。

(import (liii check) (liii lang) (liii option))

(check-set-mode! 'report-failed)

;; === :map（None 短路）===
(check ($ (option 2) :map (* _ 10)) => (option 20))
(check ($ (none) :map (* _ 10)) => (none))

;; === :filter（谓词不满足转 None）===
(check ($ (option 5) :filter (> _ 3)) => (option 5))
(check ($ (option 2) :filter (> _ 3)) => (none))
(check ($ (none) :filter (> _ 3)) => (none))

;; === :flat-map（f 须返回 option）===
(check
  ($ (option 5) :flat-map (lambda (x) (option (* x 2))))
  =>
  (option 10)
) ;check
(check
  ($ (none) :flat-map (lambda (x) (option (* x 2))))
  =>
  (none)
) ;check

;; === :get-or-else（终结，回退默认值）===
(check ($ (option 5) :get-or-else 0) => 5)
(check ($ (none) :get-or-else 0) => 0)

;; === 链式组合 ===
(check ($ (option 2) :map (* _ 10) :get-or-else -1) => 20)
(check ($ (none) :map (* _ 10) :get-or-else -1) => -1)
(check ($ (option 5) :filter (> _ 3) :map (* _ 2) :get-or-else 0) => 10)
(check ($ (option 2) :filter (> _ 3) :map (* _ 2) :get-or-else 0) => 0)

;; === :exists / :forall（None: exists=#f, forall=#t 空真）===
(check ($ (option 1) :exists (> _ 0)) => #t)
(check ($ (option 1) :exists (< _ 0)) => #f)
(check ($ (none) :exists (> _ 0)) => #f)
(check ($ (option 1) :forall (> _ 0)) => #t)
(check ($ (option 1) :forall (< _ 0)) => #f)
(check ($ (none) :forall (> _ 0)) => #t)

;; === :contains ===
(check ($ (option 1) :contains 1) => #t)
(check ($ (option 1) :contains 2) => #f)
(check ($ (none) :contains 1) => #f)

;; === :or-else（空 option 回退备用 option，支持续链）===
(check ($ (option 1) :or-else (option 2)) => (option 1))
(check ($ (none) :or-else (option 2)) => (option 2))
(check ($ (none) :or-else (none)) => (none))
(check ($ (none) :or-else (option 5) :map (* _ 10) :get-or-else 0) => 50)

(check-report)
