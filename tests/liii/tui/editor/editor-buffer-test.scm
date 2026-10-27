(import (liii check)
        (liii tui editor buffer)
        (liii tui editor kill))

(check-set-mode! 'report-failed)

;; make-buffer & 字符插入与删除操作测试

;; 1. 初始状态
(let ((b (make-buffer)))
  (check (buffer? b) => #t)
  (check (buffer-lines b) => '(""))
  (check (buffer-row b) => 0)
  (check (buffer-col b) => 0)
  (check (buffer-text b) => ""))

;; 2. 插入 ASCII 字符与字符串
(let ((b (make-buffer "" #f)))
  (buffer-insert-string! b "hello")
  (check (buffer-text b) => "hello")
  (check (buffer-col b) => 5)
  (buffer-insert-char! b #\space)
  (buffer-insert-string! b "world")
  (check (buffer-text b) => "hello world")
  (check (buffer-col b) => 11))

;; 3. 插入 CJK 汉字 (Unicode 字符计数)
(let ((b (make-buffer "" #f)))
  (buffer-insert-string! b "金鱼")
  (check (buffer-text b) => "金鱼")
  (check (buffer-col b) => 2)
  (buffer-insert-string! b "TUI")
  (check (buffer-text b) => "金鱼TUI")
  (check (buffer-col b) => 5))

;; 3.1 CJK 汉字整字退格与删除
(let ((b (make-buffer "金鱼" #f)))
  (buffer-col-set! b 2)
  ;; 退格删除整字 "鱼"
  (check (buffer-backspace! b) => #t)
  (check (buffer-text b) => "金")
  (check (buffer-col b) => 1)
  ;; 退格删除整字 "金"
  (check (buffer-backspace! b) => #t)
  (check (buffer-text b) => "")
  (check (buffer-col b) => 0)
  (check (buffer-backspace! b) => #f))

(let ((b (make-buffer "金鱼Scheme" #f)))
  ;; 光标在 "金" 和 "鱼" 之间 (col=1)
  (buffer-col-set! b 1)
  ;; Delete 删除 "鱼"
  (check (buffer-delete! b) => #t)
  (check (buffer-text b) => "金Scheme")
  (check (buffer-col b) => 1))

;; 4. 行内 Backspace 退格删除
(let ((b (make-buffer "abc" #f)))
  (buffer-col-set! b 3)
  (check (buffer-backspace! b) => #t)
  (check (buffer-text b) => "ab")
  (check (buffer-col b) => 2)
  ;; 中间退格
  (buffer-col-set! b 1)
  (check (buffer-backspace! b) => #t)
  (check (buffer-text b) => "b")
  (check (buffer-col b) => 0)
  ;; 在行首退格（单行模式无效）
  (check (buffer-backspace! b) => #f)
  (check (buffer-text b) => "b"))

;; 5. 行内 Delete 向后删除
(let ((b (make-buffer "xyz" #f)))
  (buffer-col-set! b 1)
  (check (buffer-delete! b) => #t)
  (check (buffer-text b) => "xz")
  (check (buffer-col b) => 1)
  ;; 行尾 Delete 无效
  (buffer-col-set! b 2)
  (check (buffer-delete! b) => #f)
  (check (buffer-text b) => "xz"))

;; 6. 多行模式下的 Backspace 合并行
(let ((b (make-buffer "line1\nline2" #t)))
  (buffer-row-set! b 1)
  (buffer-col-set! b 0)
  ;; 在第二行行首按退格，第二行应拼接到第一行末尾
  (check (buffer-backspace! b) => #t)
  (check (buffer-lines b) => '("line1line2"))
  (check (buffer-row b) => 0)
  (check (buffer-col b) => 5))

;; 7. 多行模式下的 Delete 合并行
(let ((b (make-buffer "abc\ndef" #t)))
  (buffer-row-set! b 0)
  (buffer-col-set! b 3)
  ;; 在第一行行尾按 Delete，第二行应拼接到第一行末尾
  (check (buffer-delete! b) => #t)
  (check (buffer-lines b) => '("abcdef"))
  (check (buffer-row b) => 0)
  (check (buffer-col b) => 3))

;; 三行以上缓冲区在合并中间行时保留前后行及顺序。
(let ((b (make-buffer "first\nsecond\nthird" #t)))
  (buffer-row-set! b 1)
  (buffer-col-set! b 0)
  (check (buffer-backspace! b) => #t)
  (check (buffer-lines b) => '("firstsecond" "third"))
  (check (buffer-row b) => 0)
  (check (buffer-col b) => 5))

(let ((b (make-buffer "first\nsecond\nthird" #t)))
  (buffer-row-set! b 1)
  (buffer-col-set! b 6)
  (check (buffer-delete! b) => #t)
  (check (buffer-lines b) => '("first" "secondthird"))
  (check (buffer-row b) => 1)
  (check (buffer-col b) => 6))

(let ((b (make-buffer "first\nsecond\nthird" #t)))
  (buffer-row-set! b 1)
  (buffer-col-set! b 3)
  (check (buffer-newline! b) => #t)
  (check (buffer-lines b) => '("first" "sec" "ond" "third"))
  (check (buffer-row b) => 2)
  (check (buffer-col b) => 0))

;; 行替换覆盖首行、中间行、末行及越界索引。
(check (%replace-line '("one" "two" "three") 0 "first") =>
  '("first" "two" "three"))
(check (%replace-line '("one" "two" "three") 1 "second") =>
  '("one" "second" "three"))
(check (%replace-line '("one" "two" "three") 2 "third") =>
  '("one" "two" "third"))
(check (%replace-line '("one" "two" "three") 3 "missing") =>
  '("one" "two" "three"))

;; 8. 行与词删除操作测试：kill-line-right! / kill-line-left! / kill-word-left! / transpose-chars!
(let ((b (make-buffer "hello world" #f)))
  (buffer-col-set! b 5)
  ;; ctrl-k: 删除光标到行尾
  (check (buffer-kill-line-right! b) => #t)
  (check (buffer-text b) => "hello")
  (check (buffer-col b) => 5))

(let ((b (make-buffer "hello world" #f)))
  (buffer-col-set! b 6)
  ;; ctrl-u: 删除光标到行首
  (check (buffer-kill-line-left! b) => #t)
  (check (buffer-text b) => "world")
  (check (buffer-col b) => 0))

(let ((b (make-buffer "hello world" #f)))
  (buffer-col-set! b 11)
  ;; ctrl-w: 向左删除单词
  (check (buffer-kill-word-left! b) => #t)
  (check (buffer-text b) => "hello ")
  (check (buffer-col b) => 6))

(let ((b (make-buffer "ab" #f)))
  (buffer-col-set! b 2)
  ;; ctrl-t: 交换光标前两个字符
  (check (buffer-transpose-chars! b) => #t)
  (check (buffer-text b) => "ba")
  (check (buffer-col b) => 2))

(check-report)
