;; screen-diff
;; 比较两帧文本行并生成行级差分操作序列。
;;
;; 语法
;; ----
;; (screen-diff old-lines new-lines)
;;
;; 参数
;; ----
;; old-lines : list?
;; 上一帧的文本行列表。
;;
;; new-lines : list?
;; 当前帧的文本行列表。
;;
;; 返回值
;; ----
;; list?
;; 差分操作列表，每项形如 (keep index)、(update index text)、(append index text) 或 (clear index)。

(import (scheme base) (liii check) (liii tui main-screen diff))

(check-set-mode! 'report-failed)

;; 空列表比较
(check (screen-diff '() '()) => '())

;; 新增行 (append)
(check (screen-diff '() '("a" "b")) => '((append 0 "a") (append 1 "b")))

;; 清除行 (clear)
(check (screen-diff '("a" "b") '()) => '((clear 0) (clear 1)))

;; 完全相同行 (keep)
(check (screen-diff '("a" "b") '("a" "b")) => '((keep 0) (keep 1)))

;; 局部修改行 (update)
(check (screen-diff '("a" "b" "c") '("a" "x" "c"))
  =>
  '((keep 0) (update 1 "x") (keep 2))
) ;check

;; 末尾追加 (keep + append)
(check (screen-diff '("a" "b") '("a" "b" "c"))
  =>
  '((keep 0) (keep 1) (append 2 "c"))
) ;check

;; 行数收缩 (keep + clear)
(check (screen-diff '("a" "b" "c") '("a" "b"))
  =>
  '((keep 0) (keep 1) (clear 2))
) ;check

;; 混合更新与追加
(check (screen-diff '("a" "b") '("x" "b" "z"))
  =>
  '((update 0 "x") (keep 1) (append 2 "z"))
) ;check

;; 混合更新与清除
(check (screen-diff '("a" "b" "c") '("x"))
  =>
  '((update 0 "x") (clear 1) (clear 2))
) ;check

;; 长列表保持逐行操作语义，覆盖线性遍历实现的公共前缀与两个尾部。
(let ((old-lines (make-list 2000 "same")) (new-lines (make-list 2001 "same")))
  (let ((ops (screen-diff old-lines new-lines)))
    (check (length ops) => 2001)
    (check (car ops) => '(keep 0))
    (check (list-ref ops 1999) => '(keep 1999))
    (check (list-ref ops 2000) => '(append 2000 "same"))
  ) ;let
) ;let

;; 类型错误测试
(check-catch 'type-error (screen-diff "invalid" '()))
(check-catch 'type-error (screen-diff '() "invalid"))
(check-catch 'type-error (screen-diff '(123) '("ok")))
(check-catch 'type-error (screen-diff '("ok") '(123)))

(check-report)
