(import (liii check)
        (liii tui editor buffer))

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

;; 3. 插入 CJK 汉字 (UTF-8 字符字节长度为 6，字符数/显示宽度因底层 string-length 为字节长度)
(let ((b (make-buffer "" #f)))
  (buffer-insert-string! b "金鱼")
  (check (buffer-text b) => "金鱼")
  (check (buffer-col b) => (string-length "金鱼"))
  (buffer-insert-string! b "TUI")
  (check (buffer-text b) => "金鱼TUI")
  (check (buffer-col b) => (+ (string-length "金鱼") (string-length "TUI"))))

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

(check-report)
