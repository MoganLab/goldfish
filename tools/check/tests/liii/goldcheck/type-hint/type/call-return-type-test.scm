;; call-return-type
;; 计算候选签名中与实参兼容签名的公共返回类型。
;;
;; 语法
;; ----
;; (call-return-type sigs expr env locals)
;;
;; 参数
;; ----
;; sigs : list      候选签名列表
;; expr : list      调用表达式
;; env : alist      类型环境
;; locals : alist   局部类型绑定
;;
;; 返回值
;; ------
;; symbol、联合类型列表或 #f
;; 兼容签名的公共返回类型；无兼容签名或返回类型不一致时 #f。

(import (scheme base) (liii check) (liii goldcheck type-hint type))

(check-set-mode! 'report-failed)

(define env
  (list (cons 'convert '((string? -> integer?) (integer? -> string?)))
        (cons 'same '((string? -> integer?) (string? string? -> integer?)))
       ) ;list
) ;define

;; 1. 多签名筛选：字符串实参匹配第一分支，返回 integer?
(check (call-return-type (cdr (assq 'convert env)) '(convert "x") env '()) => 'integer?)
(check (call-return-type (cdr (assq 'convert env)) '(convert 1) env '()) => 'string?)

;; 2. 无兼容签名（数量均不符）返回 #f
(check (call-return-type (cdr (assq 'convert env)) '(convert #t #f) env '()) => #f)

;; 3. 无候选签名返回 #f
(check (call-return-type '() '(f 1) env '()) => #f)

(check-report)
