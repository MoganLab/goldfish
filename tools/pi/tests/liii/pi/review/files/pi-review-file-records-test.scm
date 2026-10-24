(import (liii check)
        (liii string)
        (liii pi review files)
) ;import

(check-set-mode! 'report-failed)

(define (fixture name)
  (let loop
    ((candidates (list (string-append "tools/pi/tests/fixtures/code-review/" name)
                       (string-append "tests/fixtures/code-review/" name)
                       (string-append "../../fixtures/code-review/" name)
                     ) ;list
     ) ;candidates
    ) ;
    (cond ((null? candidates) name)
          ((file-exists? (car candidates)) (car candidates))
          (else (loop (cdr candidates)))
    ) ;cond
  ) ;let
) ;define

(define sample-ts (fixture "sample.ts"))
(define sample-py (fixture "sample.py"))
(define missing-ts (fixture "missing.ts"))

;; pi-review-file-records
;; 读取待审查文件内容并附带语言信息；缺失文件降级为空内容。
(let ((recs (pi-review-file-records (list sample-ts sample-py missing-ts))))
  (check (length recs) => 3)
  (check (cdr (assoc 'path (list-ref recs 0))) => sample-ts)
  (check (cdr (assoc 'language (list-ref recs 0))) => "typescript")
  (check-true (string-contains? (cdr (assoc 'content (list-ref recs 0))) "const x"))
  (check (cdr (assoc 'path (list-ref recs 1))) => sample-py)
  (check (cdr (assoc 'language (list-ref recs 1))) => "python")
  (check-true (string-contains? (cdr (assoc 'content (list-ref recs 1))) "def f"))
  (check (cdr (assoc 'language (list-ref recs 2))) => "typescript")
  (check (cdr (assoc 'content (list-ref recs 2))) => "")
) ;let

(check (pi-review-file-records '()) => '())

(check-report)
