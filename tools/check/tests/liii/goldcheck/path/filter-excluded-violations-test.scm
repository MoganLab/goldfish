;; filter-excluded-violations
;; 过滤掉命中 exclude 列表的违规项。违规项形如 (rule path line ...)，
;; 其 path 既可能是被检查的文件，也可能是规则推导出的期望路径
;; （如缺失的 README.md、缺失的单元测试文件）。
;;
;; 语法
;; ----
;; (filter-excluded-violations violations excludes)
;;
;; 参数
;; ----
;; violations : list
;; 违规项列表。
;; excludes : list
;; 豁免规则列表，元素可以是字符串或对象。
;;
;; 返回值
;; ------
;; list
;; 保留未命中任何豁免规则的违规项。

(import (scheme base) (liii check) (liii list) (liii goldcheck path))

(check-set-mode! 'report-failed)

;; 1. 排除列表为空时原样返回
(let ((viols (list (list 'unit-test "a/b-test.scm" 1 "缺少【语法】段落")))) ;
  (check (filter-excluded-violations viols '()) => viols)
) ;let

;; 2. 命中排除项的违规项被过滤，未命中的保留
(let* ((viols (list (list 'unit-test "tools/fmt/tests/liii/goldfmt/README.md" 1 "缺少库导览文档 README.md")
                     (list 'unit-test "library/tests/liii/goldtool/cache/README.md" 1 "缺少库导览文档 README.md")))
                    ;list
       (excludes (list "tools/fmt/tests/liii/goldfmt/"))
      ) ;
  (check (filter-excluded-violations viols excludes)
    =>
    (list (list 'unit-test "library/tests/liii/goldtool/cache/README.md" 1 "缺少库导览文档 README.md"))
  ) ;check
) ;let*

;; 3. 排除项支持对象写法（path + reason）
(let* ((viols (list (list 'unit-test "tools/pi/tests/liii/pi/code/core/tools/x-test.scm" 1 "缺少【语法】段落")))
       (excludes (list '((path . "tools/pi/tests/liii/pi/code/core/tools") (reason . "聚合测试目录"))))
      ) ;
  (check (filter-excluded-violations viols excludes) => '())
) ;let*

(check-report)
