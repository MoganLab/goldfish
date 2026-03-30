(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-binary-search
;; 二分查找。
;;
;; 语法
;; ----
;; (flexvector-binary-search fv value cmp)
;; (flexvector-binary-search fv value cmp start)
;; (flexvector-binary-search fv value cmp start end)
;;
(let ((fv (flexvector #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j))
      (cmp (lambda (char1 char2)
             (cond ((char<? char1 char2) -1)
                   ((char=? char1 char2) 0)
                   (else 1)))))
  (check (flexvector-binary-search fv #\d cmp) => 3)
  (check (flexvector-binary-search fv #\a cmp) => 0)
  (check (flexvector-binary-search fv #\j cmp) => 9)
  (check (flexvector-binary-search fv #\k cmp) => #f)
  (check (flexvector-binary-search fv #\f cmp 2 6) => 5)
  (check (flexvector-binary-search fv #\f cmp 1 5) => #f))

(check-report)
