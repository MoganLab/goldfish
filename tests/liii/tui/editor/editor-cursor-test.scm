(import (liii check)
        (liii tui editor buffer)
        (liii tui editor cursor))

(check-set-mode! 'report-failed)

;; 光标左右移动、Home/End、多行上下移动与列吸附测试

;; 1. 单行模式左右移动
(let ((b (make-buffer "hello" #f)))
  (buffer-col-set! b 0)
  (check (buffer-move-left! b) => #f)
  (check (buffer-col b) => 0)

  (check (buffer-move-right! b) => #t)
  (check (buffer-col b) => 1)

  (check (buffer-move-right! b) => #t)
  (check (buffer-col b) => 2)

  (check (buffer-move-end! b) => #t)
  (check (buffer-col b) => 5)
  (check (buffer-move-right! b) => #f)

  (check (buffer-move-home! b) => #t)
  (check (buffer-col b) => 0))

;; 2. 多行模式下的左右跨行移动
(let ((b (make-buffer "ab\ncd" #t)))
  (buffer-row-set! b 0)
  (buffer-col-set! b 2)
  ;; 在第 0 行行尾向右，跨入第 1 行行首
  (check (buffer-move-right! b) => #t)
  (check (buffer-row b) => 1)
  (check (buffer-col b) => 0)
  ;; 在第 1 行行首向左，跨回第 0 行行尾
  (check (buffer-move-left! b) => #t)
  (check (buffer-row b) => 0)
  (check (buffer-col b) => 2))

;; 3. 多行模式下的上下移动与吸附
(let ((b (make-buffer "very long line\nshort\nanother line" #t)))
  ;; 光标在第 0 行 col 10
  (buffer-row-set! b 0)
  (buffer-col-set! b 10)

  ;; 无法再向上
  (check (buffer-move-up! b) => #f)
  (check (buffer-row b) => 0)

  ;; 向下移动到 short (长度 5)，列应吸附到 5
  (check (buffer-move-down! b) => #t)
  (check (buffer-row b) => 1)
  (check (buffer-col b) => 5)

  ;; 再次向下移动到 another line (长度 12)，列保持 5
  (check (buffer-move-down! b) => #t)
  (check (buffer-row b) => 2)
  (check (buffer-col b) => 5)

  ;; 无法再向下
  (check (buffer-move-down! b) => #f)
  (check (buffer-row b) => 2)

  ;; 向上移动回 short，列吸附到 5
  (check (buffer-move-up! b) => #t)
  (check (buffer-row b) => 1)
  (check (buffer-col b) => 5))

;; 4. 回车换行分裂测试
(let ((b (make-buffer "hello world" #t)))
  (buffer-col-set! b 5)
  (check (buffer-newline! b) => #t)
  (check (buffer-lines b) => '("hello" " world"))
  (check (buffer-row b) => 1)
  (check (buffer-col b) => 0))

(check-report)
