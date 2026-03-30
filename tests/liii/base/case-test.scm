(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; case
;; 测试 case 分支表达式。
;;
;; 语法
;; ----
;; (case key clause ...)
;;
;; 参数
;; ----
;; key : any?
;; clause : case 子句
;;
;; 返回值
;; ----
;; any?
;; 返回与 key 匹配的子句结果；未命中时返回未指定值。
;;
;; 注意
;; ----
;; 本文件保留原聚合测试中的符号匹配场景。
;;
;; 示例
;; ----
;; (case '+ ((+ -) 'p0) ((* /) 'p1)) => 'p0
;;
;; 错误处理
;; ----
;; 按命中子句中表达式自身规则处理

(check (case '+
         ((+ -) 'p0)
         ((* /) 'p1))
  => 'p0
) ;check

(check (case '-
         ((+ -) 'p0)
         ((* /) 'p1))
  => 'p0
) ;check

(check (case '*
         ((+ -) 'p0)
         ((* /) 'p1))
  => 'p1
) ;check

(check (case '@
         ((+ -) 'p0)
         ((* /) 'p1))
  => #<unspecified>
) ;check

(check (case '&
         ((+ -) 'p0)
         ((* /) 'p1))
  => #<unspecified>
) ;check

(check-report)
