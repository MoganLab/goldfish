(import (liii check) (liii pi code core tools grep format))

(check-set-mode! 'report-failed)

;; %format-grep-matches
;; 将 grep 匹配位置及文件行缓存格式化为带上下文的文本结果。
;;
;; 语法
;; ----
;; (%format-grep-matches matches cache context limit limit-reached? skipped) -> string
;;
;; 参数
;; ----
;; matches : list
;; (relative-path line-number) 形式的匹配位置列表。
;; cache : alist
;; 文件相对路径到行列表的映射。
;; context : integer
;; 匹配行前后显示的上下文行数。
;; limit : integer
;; 结果数量限制，用于生成提示。
;; limit-reached? : boolean
;; 是否达到结果数量限制。
;; skipped : list
;; 因不可读或已删除而跳过的文件路径。
;;
;; 返回值
;; ----
;; string
;; 匹配行使用冒号分隔，上下文行使用连字符分隔，末尾附必要的截断或跳过提示。
;;
;; 示例
;; ----
;; (%format-grep-matches '(("a.scm" 2)) cache 1 100 #f '())

(let ((cache '(("a.scm" "one" "two" "three"))))
  (check (%format-grep-matches '(("a.scm" 2)) cache 1 100 #f '())
    =>
    "a.scm-1- one\na.scm:2: two\na.scm-3- three"
  ) ;check
) ;let

(let ((cache '(("a.scm" "one" "two"))))
  (check (%format-grep-matches '(("a.scm" 1)) cache 0 1 #t '())
    =>
    "a.scm:1: one\n\n[1 matches limit reached. Use limit=2 for more, or refine pattern]"
  ) ;check
) ;let

(check-report)
