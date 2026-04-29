(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-index-right
;; 从右向左查找，返回满足谓词的字符的下一个位置（successor）的cursor。
;; 如果没有找到，返回 start cursor。
;;
;; 语法
;; ----
;; (string-index-right s pred [start end])
;;
;; 参数
;; ----
;; s : string
;; 要搜索的字符串
;;
;; pred : procedure
;; 一元字符谓词函数
;;
;; start : integer (可选)
;; 搜索起始位置（字符索引），默认为0
;;
;; end : integer (可选)
;; 搜索结束位置（字符索引），默认为字符串字符数
;;
;; 返回值
;; ------
;; string-cursor?
;; 找到则返回 successor cursor，否则返回 start cursor
;;
;; 说明
;; ----
;; 1. 搜索范围是 [start, end)，包含 start，不包含 end
;; 2. 注意从右向左搜索时，第一个考虑的位置是 (string-cursor-prev s end)
;; 3. 返回的是 successor cursor（匹配字符的下一个位置）
;; 4. 适用于 ASCII、中文、emoji 等各种 Unicode 字符
;; 5. 性能：O(n)，n 为字符串字符数
;;
;; 错误处理
;; --------
;; value-error
;; 当 start/end 超出范围时抛出错误

;; 基本测试 - ASCII
(let ((s "abc123"))
  ;; 最右边的字母是 c，索引2，successor是索引3
  (check (string-cursor->index s (string-index-right s char-alphabetic?)) => 3)
  ;; 最右边的数字是 3，索引5，successor是索引6（end）
  (check (string-cursor->index s (string-index-right s char-numeric?)) => 6)
) ;let

;; 测试未找到的情况
(let ((s "abc"))
  (check (string-cursor->index s (string-index-right s char-numeric?)) => 0)
) ;let

;; 测试带 start/end 参数
(let ((s "abc123"))
  ;; 在 [0, 4) 范围内，最右边的字母是 c（索引2），successor是3
  (check (string-cursor->index s (string-index-right s char-alphabetic? 0 4))
    =>
    3
  ) ;check
  ;; 在 [3, 6) 范围内，最右边的数字是 3（索引5），successor是6
  (check (string-cursor->index s (string-index-right s char-numeric? 3 6)) => 6)
) ;let

;; 测试空字符串
(let ((s ""))
  (check (string-cursor->index s (string-index-right s char-alphabetic?)) => 0)
) ;let

;; 测试中文
(let ((s "中文abc"))
  ;; 最右边的字母是 c，索引4（中=0, 文=1, a=2, b=3, c=4），successor是5
  (check (string-cursor->index s (string-index-right s char-alphabetic?)) => 5)
) ;let


;; 测试使用游标作为 start/end
(let* ((s "abc123")
       (start (string-cursor-start s))
       (end (string-cursor-end s))
       (result (string-index-right s char-alphabetic? start end))
      ) ;
  (check (string-cursor->index s result) => 3)
) ;let*
(check-report)
