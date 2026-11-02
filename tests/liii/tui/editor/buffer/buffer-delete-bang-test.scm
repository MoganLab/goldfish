;; buffer-delete!
;; 在光标当前位置执行向后删除（Delete）操作。
;;
;; 语法
;; ----
;; (buffer-delete! b)
;;
;; 参数
;; ----
;; b : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; boolean?
;; 若成功删除了字符或合并了后续行返回 #t，处于末尾无法删除时返回 #f。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

;; 1. 行内删除
(let ((b (make-buffer "abc" #f)))
  (buffer-col-set! b 1)
  (check (buffer-delete! b) => #t)
  (check (buffer-text b) => "ac")
  (check (buffer-col b) => 1)
) ;let

;; 2. CJK 字符删除
(let ((b (make-buffer "金鱼" #f)))
  (buffer-col-set! b 0)
  (check (buffer-delete! b) => #t)
  (check (buffer-text b) => "鱼")
) ;let

;; 3. 行尾删除（单行返回 #f）
(let ((b (make-buffer "abc" #f)))
  (buffer-col-set! b 3)
  (check (buffer-delete! b) => #f)
) ;let

;; 4. 多行模式下在行尾删除将下一行合并上来
(let ((b (make-buffer "one\ntwo" #t)))
  (buffer-row-set! b 0)
  (buffer-col-set! b 3)
  (check (buffer-delete! b) => #t)
  (check (buffer-text b) => "onetwo")
) ;let

(check-report)
