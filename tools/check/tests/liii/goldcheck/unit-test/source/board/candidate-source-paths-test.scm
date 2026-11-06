;; candidate-source-paths
;; 按库标识符列出各板块根目录下的候选源码文件路径。
;;
;; 语法
;; ----
;; (candidate-source-paths base-dir spec)
;;
;; 参数
;; ----
;; base-dir : string
;; 源码所在工程根目录（仓库根或临时夹具根）。
;;
;; spec : list of symbol
;; 库标识符符号列表，如 '(scheme base)。
;;
;; 返回值
;; ------
;; list of string
;; 候选源码路径列表：先依次尝试 base-dir 下的 goldfish/ r7rs/ library/，
;; 再依次尝试仓库根下的同名板块目录。

(import (scheme base)
  (liii check)
  (liii path)
  (liii string)
  (liii tempfile)
  (liii goldcheck unit-test source board)
) ;import

(check-set-mode! 'report-failed)

;; 夹具工程根：用 path-join 构造期望路径，避免依赖平台路径分隔符

(define (fixture-path root board)
  (path->string (path-join root board "scheme/base.scm"))
) ;define

(define (contains? needle xs)
  (if (member needle xs string=?) #t #f)
) ;define

(define (index-of needle xs)
  (let loop
    ((i 0) (rest xs))
    (cond ((null? rest) -1)
          ((string=? (car rest) needle) i)
          (else (loop (+ i 1) (cdr rest)))
    ) ;cond
  ) ;let
) ;define

;; 1. 候选路径覆盖各板块，且工程根与仓库根各给出三个板块
;; 2. 候选顺序：工程根的三个板块先于仓库根的三个板块
(with-temp-dir
  (lambda (root)
    (let ((paths (candidate-source-paths root '(scheme base))))
      (check (length paths) => 6)
      (check-true (contains? (fixture-path root "goldfish") paths))
      (check-true (contains? (fixture-path root "r7rs") paths))
      (check-true (contains? (fixture-path root "library") paths))
      (check-true (< (index-of (fixture-path root "goldfish") paths)
                    (index-of (fixture-path root "r7rs") paths)
                  ) ;<
      ) ;check-true
      (check-true (< (index-of (fixture-path root "r7rs") paths)
                    (index-of (fixture-path root "library") paths)
                  ) ;<
      ) ;check-true
    ) ;let
  ) ;lambda
  "gf-board-"
) ;with-temp-dir

(check-report)
