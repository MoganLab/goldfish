(import (liii check)
  (liii match)
  (liii packrat))

(check-set-mode! 'report-failed)

;; [1132] 回归：match-gen-ellipsis 恢复无首参签名、..1 由独立的
;; match-gen-ellipsis-1 承载，避免 syntax-rules 展开器在嵌套库加载
;; 后于数字字面量子句上产生 <free cell!> 崩溃。
;; 本文件验证 ..1 语义不回归（详细崩溃场景见 PR 描述）。
(check (match '(1) ((a ..1) a)) => '(1))
(check (match (list 1 2 3) ((a ..1) a) (_ 'no)) => '(1 2 3))
(check (match '() ((a ..1) a) (_ 'ok)) => 'ok)
(check (match 42 ((a ..1) a) (_ 'ok)) => 'ok)
(check (match '(1 . 2) ((a ..1) a) (_ 'improper)) => 'improper)
(check (match '(1 2 3 4) ((x ... y) (list x y))) => '((1 2 3) 4))

;; 嵌套库加载后展开器仍正常（轻量模拟：packrat 内部使用 match）
(check (parse-result? (make-result 42 #f)) => #t)

(check-report)
