;; reachable-hint-files
;; 返回检查目标文件时实际加载类型环境的 hint 库文件列表（去重）。
;;
;; 语法
;; ----
;; (reachable-hint-files files)
;;
;; 参数
;; ----
;; files : list of string  目标源码/hint 文件路径列表
;;
;; 返回值
;; ------
;; list of string
;; 相对仓库根的 hint 库文件路径列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldcheck type-hint target)
)

(check-set-mode! 'report-failed)

;; 返回的 hint 路径可能带仓库根前缀，按后缀断言
(define (has-suffix-path? files suffix)
  (any (lambda (f) (string-ends? f suffix)) files)
) ;define

(define (target-file rel)
  ;; cwd 可能是仓库根或 tools/check：优先仓库根形态
  (cond ((file-exists? rel) rel)
        ((file-exists? (string-append "tools/check/" rel)) (string-append "tools/check/" rel))
        (else rel)
  ) ;cond
) ;define

;; 1. 源码文件：自身 + import 闭包的 hint + scheme base 内建层
(let ((files (reachable-hint-files (list (target-file "liii/goldcheck/path.scm")))))
  (check-true (list? files))
  (check-true (every string? files))
  (check-true (has-suffix-path? files "tools/check/hint/liii/goldcheck/path.scm"))
  (check-true (has-suffix-path? files "r7rs/hint/scheme/base.scm"))
) ;let

;; 2. hint 文件：自身 + 原库 import 闭包的 hint
(let ((files (reachable-hint-files (list (target-file "hint/liii/goldcheck/path.scm")))))
  (check-true (list? files))
  (check-true (has-suffix-path? files "tools/check/hint/liii/goldcheck/path.scm"))
  (check-true (has-suffix-path? files "r7rs/hint/scheme/base.scm"))
) ;let

;; 3. 非库文件不产生可达 hint
(let ((files (reachable-hint-files (list (target-file "gf_check.json")))))
  (check files => '())
) ;let

;; 4. 多文件目标去重
(let ((files
        (reachable-hint-files
          (list (target-file "liii/goldcheck/path.scm")
            (target-file "hint/liii/goldcheck/path.scm")
          ) ;list
        ) ;reachable-hint-files
      ) ;files
    )
  (check-true
    (= 1
      (length
        (filter
          (lambda (f) (string-ends? f "tools/check/hint/liii/goldcheck/path.scm"))
          files
        ) ;filter
      ) ;length
    ) ;=
  ) ;check-true
) ;let

(check-report)
