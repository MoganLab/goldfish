(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-contains
;; 在 s1 中查找 s2 第一次出现的位置，返回 cursor。
;;
;; 语法
;; ----
;; (string-contains s1 s2 [start1 end1 start2 end2])
;;
;; 参数
;; ----
;; s1, s2 : string
;; 被搜索字符串和搜索字符串
;;
;; start1, end1, start2, end2 : integer 或 string-cursor? (可选)
;; 子串范围，默认为整个字符串
;;
;; 返回值
;; ------
;; string-cursor? 或 #f
;; 第一次出现位置的游标，或 #f 如果未找到
;;
;; 说明
;; ----
;; 1. string-contains 是 SRFI-130 中的字符串搜索函数
;; 2. 与 (liii string) 中的 string-contains 功能相同，但返回游标而非索引
;; 3. 性能：O(n×m)，n 为 s1 长度，m 为 s2 长度

;; 基本测试
(let ((result (string-contains "abcdef" "cd")))
  (check (string-cursor? result) => #t)
  (check (string-cursor->index "abcdef" result) => 2)
) ;let

(check (string-contains "abcdef" "xyz") => #f)
(check (string-cursor->index "abcdef" (string-contains "abcdef" "")) => 0)
(check (string-cursor->index "" (string-contains "" "")) => 0)
(check (string-contains "" "abc") => #f)

;; 测试中文
(let ((result (string-contains "我是中国人" "中国")))
  (check (string-cursor->index "我是中国人" result) => 2)
) ;let

;; 测试多次出现
(let ((result (string-contains "ababab" "ab")))
  (check (string-cursor->index "ababab" result) => 0)
) ;let


;; 测试使用整数索引作为 start/end
(let ((result (string-contains "abcdef" "cd" 1 5)))
  (check (string-cursor->index "abcdef" result) => 2)
) ;let

;; 测试使用游标作为 start/end
(let* ((s1 "abcdef")
       (s2 "cd")
       (start1 (string-cursor-start s1))
       (end1 (string-cursor-end s1))
       (start2 (string-cursor-start s2))
       (end2 (string-cursor-end s2))
       (result (string-contains s1 s2 start1 end1 start2 end2))
      ) ;
  (check (string-cursor->index s1 result) => 2)
) ;let*

;; 测试整数索引限制搜索范围
(check (string-contains "abcdef" "cd" 3 6) => #f)

;; 测试混合类型报错
(check-catch 'type-error
  (string-contains "abc" "ab" 0 (string-cursor-end "abc"))
) ;check-catch

;; 测试 start > end 报错
(check-catch 'value-error (string-contains "abc" "ab" 2 1))

;; 测试负数报错
(check-catch 'value-error (string-contains "abc" "ab" -1 2))

(check-report)
