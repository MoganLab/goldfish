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
  (liii goldcheck unit-test source board)
) ;import

(check-set-mode! 'report-failed)

(define (path-strings paths)
  (map (lambda (p) (path->string (path p))) paths)
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
(let ((paths (path-strings
               (candidate-source-paths "/tmp/gf-board-fixture" '(scheme base))
             ) ;path-strings
      )
     ) ;
  (check (length paths) => 6)
  (check-true (contains? "/tmp/gf-board-fixture/goldfish/scheme/base.scm" paths))
  (check-true (contains? "/tmp/gf-board-fixture/r7rs/scheme/base.scm" paths))
  (check-true (contains? "/tmp/gf-board-fixture/library/scheme/base.scm" paths))
) ;let

;; 2. 候选顺序：工程根的三个板块先于仓库根的三个板块
(let ((paths (path-strings
               (candidate-source-paths "/tmp/gf-board-fixture" '(scheme base))
             ) ;path-strings
      )
     ) ;
  (check-true
    (< (index-of "/tmp/gf-board-fixture/goldfish/scheme/base.scm" paths)
      (index-of "/tmp/gf-board-fixture/r7rs/scheme/base.scm" paths)
    )
  ) ;check-true
  (check-true
    (< (index-of "/tmp/gf-board-fixture/r7rs/scheme/base.scm" paths)
      (index-of "/tmp/gf-board-fixture/library/scheme/base.scm" paths)
    )
  ) ;check-true
) ;let

(check-report)
