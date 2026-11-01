(import (scheme base) (liii check) (liii string) (liii goldcheck))

;; format-violation
;; 将单条违规数据结构转换为对齐输出的描述字符串。
;;
;; 语法
;; ----
;; (format-violation viol)
;;
;; 参数
;; ----
;; viol : list
;; 违规项结构，如 (list 'max-file-lines file-path count limit)
;; 或 (list 'max-tree-depth file-path line depth limit)。
;;
;; 返回值
;; ------
;; string
;; 格式化后的违规说明文本。

(check-set-mode! 'report-failed)

;; 1. 格式化 max-file-lines 违规
(let* ((viol (list 'max-file-lines "foo/bar.scm" 305 300))
       (msg (format-violation viol))
      ) ;
  (check-true (string-contains? msg "foo/bar.scm: max-file-lines: 305 (limit 300)")
  ) ;check-true
) ;let*

;; 2. 格式化 max-tree-depth 违规（含行号）
(let* ((viol (list 'max-tree-depth "foo/deep.scm" 42 18 15))
       (msg (format-violation viol))
      ) ;
  (check-true (string-contains? msg "foo/deep.scm:42: max-tree-depth: 18 (limit 15)")
  ) ;check-true
) ;let*

;; 3. 格式化 define-record-type 违规（含行号）
(let* ((viol (list 'define-record-type "foo/rec.scm" 7
              "record type '<foo>' should not be exported")
       ) ;viol
       (msg (format-violation viol))
      ) ;
  (check-true (string-contains? msg "foo/rec.scm:7: define-record-type: record type '<foo>' should not be exported")
  ) ;check-true
) ;let*

;; 4. 格式化 no-crlf 违规（含行号）
(let* ((viol (list 'no-crlf "foo/bar.scm" 3)) (msg (format-violation viol)))
  (check-true (string-contains? msg "foo/bar.scm:3: no-crlf: CRLF line ending detected (expected LF)")
  ) ;check-true
) ;let*

;; 5. 格式化 no-glue 违规，展示对应公开绑定
(let* ((viol (list 'no-glue "foo/glue.scm" 8 'g_md5 'missing-binding))
       (msg (format-violation viol)))
  (check-true
    (string-contains? msg "foo/glue.scm:8: no-glue: g_md5 missing public binding md5"))
) ;let*

;; 6. 格式化 type-hint 违规；文件不可读时降级为无源码片段
(let* ((viol (list 'type-hint "foo/bar.scm" 9 "argument 1 of cdr requires pair?"))
       (msg (format-violation viol)))
  (check-true
    (string-contains? msg "foo/bar.scm:9: type-hint: argument 1 of cdr requires pair?"))
) ;let*

;; 7. type-hint 违规携带冲突表达式；源码行可读时输出行内容与标记
(let* ((viol (list 'type-hint "goldfish/liii/alist.scm" 6 "demo msg" '(alist? l)))
       (msg (format-violation viol)))
  (check-true (string-contains? msg "type-hint: demo msg"))
  (check-true (string-contains? msg "6 |     (define (alist? l)"))
  (check-true (string-contains? msg "^^^^^^^^^"))
) ;let*

;; 8. 格式化 unit-test 违规（含行号与原因）
(let* ((viol (list 'unit-test "tests/demo-test.scm" 1 "文档必须置于文件最顶部（在 import 之前）"))
       (msg (format-violation viol)))
  (check-true
    (string-contains? msg "tests/demo-test.scm:1: unit-test: 文档必须置于文件最顶部（在 import 之前）"))
) ;let*

(check-report)
