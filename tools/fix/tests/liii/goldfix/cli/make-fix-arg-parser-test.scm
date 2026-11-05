;; make-fix-arg-parser
;; 创建 gf fix 命令的参数解析器，预置 help、dry-run、extension、
;; changed-since 四个选项，未知选项按位置参数处理。
;;
;; 语法
;; ----
;; (make-fix-arg-parser)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; procedure? 参数解析器过程

(import (liii check) (liii base) (liii goldfix cli))

(check-set-mode! 'report-failed)

;; 测试：返回的是过程
(check-true (procedure? (make-fix-arg-parser)))

;; 测试：解析 --dry-run 选项与位置参数
(let ((parser (make-fix-arg-parser)))
  (parser :parse-args '("fix" "--dry-run" "a.scm"))
  (check (parser 'dry-run) => #t)
  (check (parser 'extension) => "scm")
  (check (parser :positionals) => '("a.scm"))
) ;let

;; 测试：未指定选项时取默认值
(let ((parser (make-fix-arg-parser)))
  (parser :parse-args '("fix"))
  (check (parser 'help) => #f)
  (check (parser 'dry-run) => #f)
  (check (parser 'changed-since) => #f)
) ;let

(check-report)
