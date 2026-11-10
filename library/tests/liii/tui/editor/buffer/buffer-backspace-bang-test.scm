;; buffer-backspace!
;; 在光标当前位置执行退格（Backspace）操作。
;;
;; 语法
;; ----
;; (buffer-backspace! b)
;;
;; 参数
;; ----
;; b : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; boolean?
;; 若成功删除了字符或合并了行返回 #t，处于文本起点无法退格时返回 #f。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

;; 1. 行内退格
(let ((b (make-buffer "abc" #f)))
  (buffer-col-set! b 3)
  (check (buffer-backspace! b) => #t)
  (check (buffer-text b) => "ab")
  (check (buffer-col b) => 2)
) ;let

;; 2. CJK 字符退格整字删除
(let ((b (make-buffer "金鱼" #f)))
  (buffer-col-set! b 2)
  (check (buffer-backspace! b) => #t)
  (check (buffer-text b) => "金")
  (check (buffer-col b) => 1)
) ;let

;; 3. 行首退格（单行返回 #f）
(let ((b (make-buffer "x" #f)))
  (buffer-col-set! b 0)
  (check (buffer-backspace! b) => #f)
) ;let

;; 4. 多行模式下在第二行首退格与前一行合并
(let ((b (make-buffer "one\ntwo" #t)))
  (buffer-row-set! b 1)
  (buffer-col-set! b 0)
  (check (buffer-backspace! b) => #t)
  (check (buffer-text b) => "onetwo")
  (check (buffer-row b) => 0)
  (check (buffer-col b) => 3)
) ;let

(check-report)
