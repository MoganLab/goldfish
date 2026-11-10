;; hint-roots-of
;; 解析给定 hint 文件路径所关联的 hint 根目录列表。
;;
;; 语法
;; ----
;; (hint-roots-of hint-file)
;;
;; 参数
;; ----
;; hint-file : string  hint 库文件路径
;;
;; 返回值
;; ------
;; list of string
;; 关联的 hint 根目录路径列表：本地 hint 根在前，
;; 各 domain 推导出的 hint 根在后（与 tests 目录同源）。

(import (scheme base)
  (liii check)
  (liii gfproject config)
  (liii goldcheck type-hint env)
) ;import

(check-set-mode! 'report-failed)

;; 保持顺序去重
(define (%dedup-strings xs)
  (let loop
    ((rest xs) (acc '()))
    (if (null? rest)
      (reverse acc)
      (if (member (car rest) acc string=?)
        (loop (cdr rest) acc)
        (loop (cdr rest) (cons (car rest) acc))
      ) ;if
    ) ;if
  ) ;let
) ;define

;; 1. goldfish/hint 下的文件：本地 hint 根在前，各 domain 的 hint 根在后
(let ((roots (hint-roots-of "goldfish/hint/liii/clean.scm")))
  (check roots => (%dedup-strings (cons "goldfish/hint" (gfproject-hint-dirs))))
  (check-true (pair? (member "library/hint" roots)))
) ;let

;; 2. r7rs/hint 下的文件
(let ((roots (hint-roots-of "r7rs/hint/scheme/base.scm")))
  (check roots => (%dedup-strings (cons "r7rs/hint" (gfproject-hint-dirs))))
) ;let

;; 3. tools 下的文件关联自身 hint 根及各 domain 的 hint 根
(let ((roots (hint-roots-of "tools/check/hint/liii/foo.scm")))
  (check roots => (%dedup-strings (cons "tools/check/hint" (gfproject-hint-dirs))))
) ;let

;; 4. library/hint 下的文件关联标准库板块的 hint 根
(let ((roots (hint-roots-of "library/hint/liii/json.scm")))
  (check roots => (%dedup-strings (cons "library/hint" (gfproject-hint-dirs))))
) ;let

;; 5. 非 hint 路径返回空列表
(check (hint-roots-of "other/path/foo.scm") => '())

(check-report)
