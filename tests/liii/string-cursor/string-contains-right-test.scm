(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-contains-right
;; 在 s1 中查找 s2 最后一次出现的位置，返回 cursor。
;;
;; 语法
;; ----
;; (string-contains-right s1 s2 [start1 end1 start2 end2])
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
;; 最后一次出现位置的游标，或 #f 如果未找到
;;
;; 说明
;; ----
;; 1. string-contains-right 是 SRFI-130 中的字符串搜索函数
;; 2. 与 (liii string) 中的 string-contains-right 功能相同，但返回游标而非索引
;; 3. 性能：O(n×m)，n 为 s1 长度，m 为 s2 长度
;; 4. 支持 Unicode 字符（包括多字节字符如中文、Emoji）的正确搜索
;;
;; 相关实现
;; --------
;; (liii string) 库中也提供了 string-contains-right 函数，返回整数索引
;; 参见: gf doc liii/string "string-contains-right"

;; 基本测试
(let ((result (string-contains-right "ababab" "ab")))
  (check (string-cursor->index "ababab" result) => 4)
) ;let

(check (string-contains-right "abcdef" "xyz") => #f)
(check (string-cursor->index "abcdef" (string-contains-right "abcdef" "")) => 6)

;; 测试中文
(let ((result (string-contains-right "中国中国" "中国")))
  (check (string-cursor->index "中国中国" result) => 2)
) ;let

;; 测试只出现一次
(let ((result (string-contains-right "abcdef" "cd")))
  (check (string-cursor->index "abcdef" result) => 2)
) ;let


;; 测试使用整数索引作为 start/end
(let ((result (string-contains-right "ababab" "ab" 0 6)))
  (check (string-cursor->index "ababab" result) => 4)
) ;let

;; 测试使用游标作为 start/end
(let* ((s1 "ababab")
        (s2 "ab")
        (start1 (string-cursor-start s1))
        (end1 (string-cursor-end s1))
        (start2 (string-cursor-start s2))
        (end2 (string-cursor-end s2))
        (result (string-contains-right s1 s2 start1 end1 start2 end2))
       ) ;
  (check (string-cursor->index s1 result) => 4)
) ;let*

;; Emoji 测试
(let ((result (string-contains-right "hello😀world😀test" "😀")))
  (check (string-cursor->index "hello😀world😀test" result) => 11)
) ;let
(check (string-contains-right "abcdef" "😀") => #f)

;; 测试混合类型报错
(check-catch 'type-error
  (string-contains-right "abc" "ab" 0 (string-cursor-end "abc"))
) ;check-catch

;; 测试 start > end 报错
(check-catch 'value-error (string-contains-right "abc" "ab" 2 1))

;; 测试负数报错
(check-catch 'value-error (string-contains-right "abc" "ab" -1 2))

(check-report)
