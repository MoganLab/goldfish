(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor-end
;; 返回字符串的post-end游标。
;;
;; 语法
;; ----
;; (string-cursor-end s)
;;
;; 参数
;; ----
;; s : string
;; 目标字符串
;;
;; 返回值
;; ------
;; string-cursor?
;; 指向字符串最后一个字符之后位置的游标
;;
;; 说明
;; ----
;; 1. 创建游标时会预扫描字符串生成偏移表
;; 2. 空字符串返回指向位置0的游标
;; 3. 性能：O(n)，n为字符串字节长度（预扫描一次）
;; 4. 支持 Unicode 字符（包括多字节字符如中文、Emoji）的正确游标定位
;;
;; 相关实现
;; --------
;; (liii string-cursor) 独有函数，无 (liii string) 对应版本
;; 参见: gf doc liii/string-cursor "string-cursor-end"

;; 测试空字符串
(let ((c (string-cursor-end "")))
  (check (string-cursor? c) => #t)
  (check (string-cursor->index "" c) => 0)
) ;let

;; 测试ASCII字符串（3个字符）
(let ((c (string-cursor-end "abc")))
  (check (string-cursor? c) => #t)
  (check (string-cursor->index "abc" c) => 3)
) ;let

;; 测试中文字符串（2个字符，但6个字节）
(let ((c (string-cursor-end "中文")))
  (check (string-cursor? c) => #t)
  (check (string-cursor->index "中文" c) => 2)
) ;let

;; 测试emoji字符串（2个字符，但8个字节）
(let ((c (string-cursor-end "🎉🎊")))
  (check (string-cursor? c) => #t)
  (check (string-cursor->index "🎉🎊" c) => 2)
) ;let

;; 测试ASCII+中文混合
(let ((c (string-cursor-end "a中b文")))
  (check (string-cursor->index "a中b文" c) => 4)
) ;let

(check-report)
