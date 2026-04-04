(import (liii check)
        (scheme base))

(check-set-mode! 'report-failed)

;; case
;; case 是 R7RS 定义的多分支条件选择表达式，根据 key 值匹配不同的子句执行。
;;
;; 说明
;; ----
;; case 是 R7RS 定义的分支表达式。
;;
;; 局限性
;; ----
;; case 使用 eqv? 进行匹配，只能精确比较值，不支持自定义匹配逻辑。
;;
;; 升级版 case*
;; ----
;; case* 是 case 的升级版，提供更灵活的匹配能力。
;; 位于 (liii case) 库中，可使用以下命令查看文档：
;;   gf doc liii/case
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
