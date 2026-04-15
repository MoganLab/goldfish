(import (liii check)
        (liii goldfmt-format)
        (liii goldfmt-scan))

(check-set-mode! 'report-failed)

;; can-inline?
;; 判断一个 node 是否可以按单行形式输出。
;;
;; 语法
;; ----
;; (can-inline? node)
;;
;; 参数
;; ----
;; node : env? 或 atom?
;; 由 `scan` 生成的格式化节点。
;;
;; 返回值
;; ------
;; boolean?
;; 返回 #t 表示该节点可以 inline，返回 #f 表示需要跨行输出。
;;
;; 说明
;; ----
;; 1. atom 总是可以 inline
;; 2. `node-rules.json` 中的 mustInline 会强制 inline
;; 3. neverInline 会禁止 inline
;; 4. neverInlineWhenFirstChildEnv 会在第一个 child 是 env 时禁止 inline
;; 5. 其他情况使用 `format-inline` 得到单行候选字符串，再和 max-inline-length 比较
;;
;; 示例
;; ----
;; (can-inline? (scan '(+ x y)))           ; => #t
;; (can-inline? (scan '(define (f x) x))) ; => #f

(check (can-inline? (scan '(+ x y))) => #t)

(check (can-inline? (scan 'x)) => #t)

(check (can-inline? (scan "hello")) => #t)

(check (can-inline? (scan '(*comment* "hello"))) => #t)

(check (can-inline? (scan '(*comment* ""))) => #t)

;; max-inline-length 当前为 40：长度等于 40 时仍可单行。
(check (can-inline? (scan '(+ aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa))) => #t)

;; 超过 max-inline-length 时不能单行。
(check (can-inline? (scan '(+ aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa))) => #f)

(check (can-inline? (scan '(unknown a b))) => #t)

(check (can-inline? (scan '(unknown aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa))) => #f)

(check (can-inline? (scan '(define x 1))) => #t)

;; define 的第一个子节点是函数签名环境时，按规则强制跨行。
(check (can-inline? (scan '(define (f x) x))) => #f)

(check (can-inline? (scan '(define-values (x) 1))) => #f)

;; begin 按规则不能单行。
(check (can-inline? (scan '(begin x))) => #f)

;; 包含 comment 的普通 env 需要跨行处理，避免输出成 (begin ;; note x)。
(check (can-inline? (scan '(begin (*comment* "note") x))) => #f)

(check-report)
