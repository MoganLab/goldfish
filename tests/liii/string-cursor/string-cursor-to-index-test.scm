(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor->index
;; 将字符串游标转换为字符索引。
;;
;; 语法
;; ----
;; (string-cursor->index str cursor)
;;
;; 参数
;; ----
;; str : string
;; 源字符串
;;
;; cursor : string-cursor?
;; 字符串游标
;;
;; 返回值
;; ------
;; integer?
;; 游标对应的字符索引位置
;;
;; 说明
;; ----
;; 1. string-cursor->index 是 SRFI-130 中的游标转换函数
;; 2. 与 (liii string) 的区别：(liii string-cursor) 提供了完整的游标操作支持
;; 3. 性能：O(1)，直接读取游标中的索引信息
;; 4. 支持 Unicode 字符（包括多字节字符如中文、Emoji）的正确索引转换
;;
;; 相关实现
;; --------
;; (liii string-cursor) 独有函数，无 (liii string) 对应版本
;; 参见: gf doc liii/string-cursor "string-cursor->index"

;; 基本测试
(let* ((s "abcdef")
       (start (string-cursor-start s))
       (end (string-cursor-end s))
       (mid (string-index->cursor s 3))
      ) ;
  (check (string-cursor->index s start) => 0)
  (check (string-cursor->index s end) => 6)
  (check (string-cursor->index s mid) => 3)
) ;let*

;; 测试使用整数索引（直接返回）
(check (string-cursor->index "abc" 2) => 2)

;; 测试中文字符串
(let* ((s "中文测试") (start (string-cursor-start s)) (end (string-cursor-end s)))
  (check (string-cursor->index s start) => 0)
  (check (string-cursor->index s end) => 4)
) ;let*

;; 测试emoji字符串
(let* ((s "🎉🎊")
       (start (string-cursor-start s))
       (c1 (string-cursor-next s start))
      ) ;
  (check (string-cursor->index s start) => 0)
  (check (string-cursor->index s c1) => 1)
) ;let*

(check-report)
