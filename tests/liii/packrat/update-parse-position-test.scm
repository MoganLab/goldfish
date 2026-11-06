;; update-parse-position
;; 按读入的字符推进解析位置。
;;
;; 语法
;; ----
;; (update-parse-position pos ch)
;;
;; 参数
;; ----
;; pos : <parse-position> 或 #f
;; 当前解析位置；为 #f 时返回 #f。
;; ch : character
;; 读入的字符。
;;
;; 返回值
;; ----
;; <parse-position> 或 #f
;; 推进后的解析位置。
;;
;; 说明
;; ----
;; 1. 普通字符列号加一。
;; 2. 回车把列号重置为 0。
;; 3. 换行行号加一并把列号重置为 0。
;; 4. 制表符把列号推进到下一个 8 的倍数。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

;; 普通字符：列号加一
(check (parse-position-column (update-parse-position (make-parse-position "f" 1 0) #\a)) => 1)

;; 回车：列号归零
(check (parse-position-column (update-parse-position (make-parse-position "f" 1 5) #\return)) => 0)

;; 换行：行号加一、列号归零
(let ((pos (update-parse-position (make-parse-position "f" 1 5) #\newline)))
  (check (parse-position-line pos) => 2)
  (check (parse-position-column pos) => 0)
) ;let

;; 制表符：推进到下一个 8 的倍数
(check (parse-position-column (update-parse-position (make-parse-position "f" 1 0) #\tab)) => 8)
(check (parse-position-column (update-parse-position (make-parse-position "f" 1 3) #\tab)) => 8)

;; 位置未知时保持 #f
(check-false (update-parse-position #f #\a))

(check-report)
