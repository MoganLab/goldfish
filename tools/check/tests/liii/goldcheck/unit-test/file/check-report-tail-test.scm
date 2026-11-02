;; check-report-tail
;; 检查测试代码末尾是否为 (check-report) 且其上方保留空行。
;;
;; 语法
;; ----
;; (check-report-tail lines rel-path)
;;
;; 参数
;; ----
;; lines : list of string   文本行列表
;; rel-path : string        文件相对路径（用于报告违规）
;;
;; 返回值
;; ------
;; list of violation
;; 违规列表，合规则返回空列表。

(import (scheme base) (liii check) (liii goldcheck unit-test file))

(check-set-mode! 'report-failed)

;; 1. 规范的末尾：(check-report) 且上方有一行空行
(let ((lines '("(check 1 => 1)" "" "(check-report)")))
  (check (check-report-tail lines "test.scm") => '())
) ;let

;; 2. (check-report) 上方无空行
(let* ((lines '("(check 1 => 1)" "(check-report)"))
       (viols (check-report-tail lines "test.scm"))
      ) ;
  (check (length viols) => 1)
  (check (list-ref (car viols) 3)
    =>
    "(check-report) 上方必须保留一行空行"
  ) ;check
) ;let*

;; 3. 最后一行为其他代码
(let* ((lines '("(check 1 => 1)" "" "(display 1)"))
       (viols (check-report-tail lines "test.scm"))
      ) ;
  (check (length viols) => 1)
  (check (list-ref (car viols) 3)
    =>
    "(check-report) 必须位于文件最后一行"
  ) ;check
) ;let*

(check-report)
