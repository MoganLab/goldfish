;; check-doc-elements
;; 校验从测试源码提取的文档注释结构是否完整且符合规范。
;;
;; 语法
;; ----
;; (check-doc-elements doc-entries rel-path)
;;
;; 参数
;; ----
;; doc-entries : list of pair  文档注释条目列表 ((line-num . line) ...)
;; rel-path : string           文件相对路径
;;
;; 返回值
;; ------
;; list of violation
;; 违规列表；若合规返回空列表 '()。

(import (scheme base) (liii check) (liii goldcheck unit-test doc))

(check-set-mode! 'report-failed)

;; 1. 规范文档无违规
(let* ((entries '((1 . ";; my-func")
                  (2 . ";; 计算某个值。")
                  (3 . ";;")
                  (4 . ";; 语法")
                  (5 . ";; ----")
                  (6 . ";; (my-func x)")
                  (7 . ";;")
                  (8 . ";; 参数")
                  (9 . ";; ----")
                  (10 . ";; x : any")
                  (11 . ";;")
                  (12 . ";; 返回值")
                  (13 . ";; ----")
                  (14 . ";; any"))
       ) ;entries
       (viols (check-doc-elements entries "test.scm"))
      ) ;
  (check viols => '())
) ;let*

;; 2. 缺少必要小节时报错
(let* ((entries '((1 . ";; my-func") (2 . ";; 语法") (3 . ";; ----")))
       (viols (check-doc-elements entries "test.scm"))
      ) ;
  (check-true (> (length viols) 0))
) ;let*

(check-report)
