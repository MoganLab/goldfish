;; %vt-handle-char!
;; 处理虚拟终端文本中的单个字符，返回处理后的下一个索引。
;;
;; 支持回车（复位列）、换行（下移一行）与普通字符（写入并右移）。
;;
;; 语法
;; ----
;; (%vt-handle-char! state text i len)
;;
;; 参数
;; ----
;; state : vt-state?
;; 虚拟终端状态实例。
;;
;; text : string?
;; 待处理的文本。
;;
;; i : integer?
;; 当前字符位置。
;;
;; len : integer?
;; 文本长度。
;;
;; 返回值
;; ----
;; integer?
;; 处理后的下一个字符索引。

(import (scheme base) (liii check) (liii tui terminal virtual state))

(check-set-mode! 'report-failed)

;; 1. 普通字符写入并右移
(let ((state (make-vt-state 5 2 0 0 #t (make-screen-lines 5 2) '() '())))
  (check (%vt-handle-char! state "ab" 0 2) => 1)
  (check (vt-cursor-x state) => 1)
  (check (vector-ref (vector-ref (vt-lines state) 0) 0) => #\a)
) ;let

;; 2. 回车复位列
(let ((state (make-vt-state 5 2 3 1 #t (make-screen-lines 5 2) '() '())))
  (check (%vt-handle-char! state "a\r" 1 2) => 2)
  (check (vt-cursor-x state) => 0)
  (check (vt-cursor-y state) => 1)
) ;let

;; 3. 换行下移一行
(let ((state (make-vt-state 5 3 0 0 #t (make-screen-lines 5 3) '() '())))
  (check (%vt-handle-char! state "\n" 0 1) => 1)
  (check (vt-cursor-x state) => 0)
  (check (vt-cursor-y state) => 1)
) ;let

;; 4. 回车加换行组合（CRLF）一次处理两个字符
(let ((state (make-vt-state 5 3 0 0 #t (make-screen-lines 5 3) '() '())))
  (check (%vt-handle-char! state "\r\n" 0 2) => 2)
  (check (vt-cursor-x state) => 0)
  (check (vt-cursor-y state) => 1)
) ;let

(check-report)
