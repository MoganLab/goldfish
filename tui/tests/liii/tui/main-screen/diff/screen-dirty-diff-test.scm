;; screen-dirty-diff
;; 在缓冲区为脏状态时生成全量更新与尾部清除的差分操作序列。
;;
;; 语法
;; ----
;; (screen-dirty-diff old-lines new-lines)
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
;; 差分操作列表，新行一律为 update 操作。

(import (scheme base) (liii check) (liii tui main-screen diff))

(check-set-mode! 'report-failed)

;; dirty diff 总是更新新行，并清除旧缓冲多出的尾部。
(check (screen-dirty-diff '("old" "stale") '("new"))
  =>
  '((update 0 "new") (clear 1))
) ;check

(check (screen-dirty-diff '() '("a" "b")) => '((update 0 "a") (update 1 "b")))

;; 内容完全相同时仍然全量更新
(check (screen-dirty-diff '("a") '("a")) => '((update 0 "a")))

(check-report)
