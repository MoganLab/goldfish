(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor-prev
;; 返回前一个cursor。
;;
;; 语法
;; ----
;; (string-cursor-prev str cursor)
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
;; 前一个游标位置
;;
;; 说明
;; ----
;; 1. string-cursor-prev 是 SRFI-130 中的游标导航函数
;; 2. 如果 cursor 是整数，会先转换为游标
;; 3. 如果已在字符串开头，会报错
;; 4. 性能：O(1)

;; 测试ASCII字符串
(let* ((s "abc")
       (end (string-cursor-end s))
       (prev1 (string-cursor-prev s end))
       (prev2 (string-cursor-prev s prev1))
      ) ;
  (check (string-cursor->index s prev1) => 2)
  (check (string-cursor->index s prev2) => 1)
) ;let*

;; 测试中文字符串
(let* ((s "中文") (end (string-cursor-end s)) (prev (string-cursor-prev s end)))
  (check (string-cursor->index s prev) => 1)
) ;let*

;; 测试emoji字符串
(let* ((s "🎉🎊") (end (string-cursor-end s)) (prev (string-cursor-prev s end)))
  (check (string-cursor->index s prev) => 1)
) ;let*

;; 测试到达start后不能再prev
(check-catch 'value-error
  (let ((start (string-cursor-start "a")))
    (string-cursor-prev "a" start)
  ) ;let
) ;check-catch

;; 测试使用整数索引
(let* ((s "abc") (prev (string-cursor-prev s 2)))
  (check (string-cursor->index s prev) => 1)
) ;let*

(check-report)
