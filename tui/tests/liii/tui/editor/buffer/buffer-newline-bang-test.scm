;; buffer-newline!
;; 在光标当前位置插入换行符，将当前行分裂为两行（仅多行模式有效）。
;;
;; 语法
;; ----
;; (buffer-newline! b)
;;
;; 参数
;; ----
;; b : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; boolean?
;; 成功分裂行返回 #t，单行模式返回 #f。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

;; 单行模式无效返回 #f
(let ((b (make-buffer "abc" #f)))
  (buffer-col-set! b 1)
  (check (buffer-newline! b) => #f)
  (check (buffer-text b) => "abc")
) ;let

;; 多行模式成功分裂
(let ((b (make-buffer "hello" #t)))
  (buffer-col-set! b 2)
  (check (buffer-newline! b) => #t)
  (check (buffer-lines b) => '("he" "llo"))
  (check (buffer-row b) => 1)
  (check (buffer-col b) => 0)
) ;let

(check-report)
