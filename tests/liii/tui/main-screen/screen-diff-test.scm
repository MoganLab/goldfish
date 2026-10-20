(import (liii check)
        (liii tui main-screen diff))

(check-set-mode! 'report-failed)

;; screen-diff: 纯函数行差分比较算法

;; 空列表比较
(check (screen-diff '() '()) => '())

;; 新增行 (append)
(check (screen-diff '() '("a" "b"))
       => '((append 0 "a") (append 1 "b")))

;; 清除行 (clear)
(check (screen-diff '("a" "b") '())
       => '((clear 0) (clear 1)))

;; 完全相同行 (keep)
(check (screen-diff '("a" "b") '("a" "b"))
       => '((keep 0) (keep 1)))

;; 局部修改行 (update)
(check (screen-diff '("a" "b" "c") '("a" "x" "c"))
       => '((keep 0) (update 1 "x") (keep 2)))

;; 末尾追加 (keep + append)
(check (screen-diff '("a" "b") '("a" "b" "c"))
       => '((keep 0) (keep 1) (append 2 "c")))

;; 行数收缩 (keep + clear)
(check (screen-diff '("a" "b" "c") '("a" "b"))
       => '((keep 0) (keep 1) (clear 2)))

;; 混合更新与追加
(check (screen-diff '("a" "b") '("x" "b" "z"))
       => '((update 0 "x") (keep 1) (append 2 "z")))

;; 混合更新与清除
(check (screen-diff '("a" "b" "c") '("x"))
       => '((update 0 "x") (clear 1) (clear 2)))

;; 辅助函数测试
(let ((op-k '(keep 0))
      (op-u '(update 1 "foo"))
      (op-a '(append 2 "bar"))
      (op-c '(clear 3)))
  (check (diff-op-type op-k) => 'keep)
  (check (diff-op-index op-k) => 0)
  (check (diff-op-text op-k) => #f)

  (check (diff-op-type op-u) => 'update)
  (check (diff-op-index op-u) => 1)
  (check (diff-op-text op-u) => "foo")

  (check (diff-op-type op-a) => 'append)
  (check (diff-op-index op-a) => 2)
  (check (diff-op-text op-a) => "bar")

  (check (diff-op-type op-c) => 'clear)
  (check (diff-op-index op-c) => 3)
  (check (diff-op-text op-c) => #f))

;; screen-diff-has-changes? 测试
(check (screen-diff-has-changes? '()) => #f)
(check (screen-diff-has-changes? '((keep 0) (keep 1))) => #f)
(check (screen-diff-has-changes? '((keep 0) (update 1 "x"))) => #t)
(check (screen-diff-has-changes? '((keep 0) (append 1 "x"))) => #t)
(check (screen-diff-has-changes? '((clear 0))) => #t)

;; 类型错误测试
(check-catch 'type-error (screen-diff "invalid" '()))
(check-catch 'type-error (screen-diff '() "invalid"))
(check-catch 'type-error (screen-diff '(123) '("ok")))
(check-catch 'type-error (screen-diff '("ok") '(123)))

(check-report)
