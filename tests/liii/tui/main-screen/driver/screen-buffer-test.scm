(import (liii check) (liii tui main-screen buffer))

(check-set-mode! 'report-failed)

;; screen-buffer 测试

(let ((buf (make-screen-buffer)))
  (check (screen-buffer? buf) => #t)
  (check (screen-buffer-lines buf) => '())
  (check (screen-buffer-prev-lines buf) => '())
  (check (screen-buffer-dirty? buf) => #t)

  ;; 修改 dirty 状态
  (screen-buffer-dirty-set! buf #f)
  (check (screen-buffer-dirty? buf) => #f)

  ;; commit 第一帧
  (screen-buffer-commit! buf '("line 1" "line 2"))
  (check (screen-buffer-lines buf) => '("line 1" "line 2"))
  (check (screen-buffer-prev-lines buf) => '())
  (check (screen-buffer-dirty? buf) => #f)

  ;; commit 第二帧
  (screen-buffer-commit! buf '("modified 1"))
  (check (screen-buffer-lines buf) => '("modified 1"))
  (check (screen-buffer-prev-lines buf) => '("line 1" "line 2"))
  (check (screen-buffer-dirty? buf) => #f)

  ;; 手动设置行
  (screen-buffer-lines-set! buf '("custom"))
  (check (screen-buffer-lines buf) => '("custom"))

  ;; 手动设置前一帧
  (screen-buffer-prev-lines-set! buf '("prev custom"))
  (check (screen-buffer-prev-lines buf) => '("prev custom"))

  ;; invalidate 使脏标记置 #t
  (screen-buffer-invalidate! buf)
  (check (screen-buffer-dirty? buf) => #t)

  ;; clear 重置行并置 dirty 为 #t
  (screen-buffer-clear! buf)
  (check (screen-buffer-lines buf) => '())
  (check (screen-buffer-dirty? buf) => #t)
) ;let

;; 带初始行创建
(let ((buf2 (make-screen-buffer '("init 1" "init 2"))))
  (check (screen-buffer? buf2) => #t)
  (check (screen-buffer-lines buf2) => '("init 1" "init 2"))
  (check (screen-buffer-prev-lines buf2) => '())
  (check (screen-buffer-dirty? buf2) => #t)
) ;let

;; 无内嵌换行时保持原列表与字符串对象，避免逐帧重建规范化结果。
(let ((lines (list "first" "第二行" "")))
  (check (eq? (screen-normalize-lines lines "test") lines) => #t)
  (check (eq? (screen-normalize-lines lines "test") lines) => #t)
) ;let

;; 有内嵌换行时仍按原语义逐行展开，并保留尾部空行。
(let ((lines (list "first" "second\nthird" "fourth\n")))
  (check (screen-normalize-lines lines "test")
    =>
    '("first" "second" "third" "fourth" "")
  ) ;check
  (check (screen-normalize-lines "one\ntwo" "test") => '("one" "two"))
  (check (screen-normalize-lines "unchanged" "test") => '("unchanged"))
) ;let

(check-catch 'type-error (screen-normalize-lines '("ok" 1) "test"))

(check-report)
