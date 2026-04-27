(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor-next
;; 返回下一个cursor。
;;
;; 语法
;; ----
;; (string-cursor-next str cursor)
;;
;; 参数
;; ----
;; str : string
;; 源字符串
;;
;; cursor : string-cursor? 或 integer?
;; 当前游标位置
;;
;; 返回值
;; ------
;; string-cursor?
;; 下一个游标位置
;;
;; 说明
;; ----
;; 1. string-cursor-next 是 SRFI-130 中的游标导航函数
;; 2. 如果 cursor 是整数，会先转换为游标
;; 3. 如果已在字符串末尾，会报错
;; 4. 性能：O(1)

;; 测试ASCII字符串
(let* ((start (string-cursor-start "abc"))
       (next1 (string-cursor-next "abc" start))
       (next2 (string-cursor-next "abc" next1))
       (end (string-cursor-end "abc")))
  (check (string-cursor->index "abc" next1) => 1)
  (check (string-cursor->index "abc" next2) => 2)
  (check (string-cursor=? next2 (string-cursor-prev "abc" end)) => #t))

;; 测试中文字符串
(let* ((start (string-cursor-start "中文"))
       (next1 (string-cursor-next "中文" start))
       (end (string-cursor-end "中文")))
  (check (string-cursor->index "中文" next1) => 1)
  (check (string-cursor=? next1 (string-cursor-prev "中文" end)) => #t))

;; 测试emoji字符串
(let* ((start (string-cursor-start "🎉🎊"))
       (next1 (string-cursor-next "🎉🎊" start))
       (end (string-cursor-end "🎉🎊")))
  (check (string-cursor->index "🎉🎊" next1) => 1)
  (check (string-cursor=? next1 (string-cursor-prev "🎉🎊" end)) => #t))

;; 测试空字符串不能有next
(check-catch 'value-error
  (string-cursor-next "" (string-cursor-start "")))

;; 测试到达end后不能再next
(check-catch 'value-error
  (let ((end (string-cursor-end "a")))
    (string-cursor-next "a" end)))

;; 测试使用整数索引
(let* ((s "abc")
       (next (string-cursor-next s 0)))
  (check (string-cursor->index s next) => 1))

(check-report)
