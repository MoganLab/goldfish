;; display-type-hint-statuses
;; 输出 hint 库的覆盖率与测试结果状态摘要到当前输出端口。
;;
;; 语法
;; ----
;; (display-type-hint-statuses hint-files violations)
;;
;; 参数
;; ----
;; hint-files : list of string  hint 库文件路径列表
;; violations : list            检查产生的违规列表
;;
;; 返回值
;; ------
;; unspecified
;; 将状态行输出到当前输出端口。

(import (scheme base)
  (scheme write)
  (liii check)
  (liii string)
  (liii goldcheck type-hint)
)

(check-set-mode! 'report-failed)

;; 测试 cwd 随运行方式不同（bin/gf test 在 tools/check，
;; --changed-since 在仓库根）：目标文件同时兼容两种 cwd；
;; 返回的 hint 路径按仓库根定位，统一按路径后缀断言
(define (has-suffix-path? files suffix)
  (any (lambda (f) (string-ends? f suffix)) files)
) ;define

(define (target-file rel)
  (if (file-exists? rel) rel (string-append "tools/check/" rel))
) ;define

;; 1. 空列表不输出任何状态行
(let ((out (with-output-to-string (lambda () (display-type-hint-statuses '() '())))))
  (check-true (string? out))
  (check out => "")
) ;let

;; 2. reachable-hint-files：源码文件自身 + import 闭包的 hint + 内建层
;;    以 (liii goldcheck path) 为目标：自身 hint 与 scheme base 内建层可达
(let ((files (reachable-hint-files (list (target-file "liii/goldcheck/path.scm")))))
  (check-true (list? files))
  (check-true (every string? files))
  (check-true (has-suffix-path? files "tools/check/hint/liii/goldcheck/path.scm"))
  (check-true (has-suffix-path? files "r7rs/hint/scheme/base.scm"))
) ;let

;; 3. hint 文件目标：自身 + 原库 import 闭包的 hint
(let ((files (reachable-hint-files (list (target-file "hint/liii/goldcheck/path.scm")))))
  (check-true (list? files))
  (check-true (has-suffix-path? files "tools/check/hint/liii/goldcheck/path.scm"))
  (check-true (has-suffix-path? files "r7rs/hint/scheme/base.scm"))
) ;let

;; 4. 非库文件不产生可达 hint
(let ((files (reachable-hint-files (list (target-file "gf_check.json")))))
  (check files => '())
) ;let

(check-report)
