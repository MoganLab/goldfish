(import (liii check)
        (liii string)
        (liii pi code simplify ranges)
) ;import

(check-set-mode! 'report-failed)

(define (fixture name)
  (let loop
    ((candidates (list (string-append "tools/pi/tests/fixtures/simplify/" name)
                       (string-append "tests/fixtures/simplify/" name)
                       (string-append "../../fixtures/simplify/" name)
                     ) ;list
     ) ;candidates
    ) ;
    (cond ((null? candidates) name)
          ((file-exists? (car candidates)) (car candidates))
          (else (loop (cdr candidates)))
    ) ;cond
  ) ;let
) ;define

(define sample (fixture "sample.ts"))

(define (make-runner table box)
  (lambda (cmd timeout)
    (vector-set! box 0 (cons cmd (vector-ref box 0)))
    (let ((entry (assoc cmd table)))
      (if entry (values (cadr entry) (caddr entry)) (values "" #t))
    ) ;let
  ) ;lambda
) ;define

(define (field key entry)
  (cdr (assoc key entry))
) ;define

;; pi-collect-simplify-files
;; 复用 044 的变更文件收集，再逐文件抓取改动行范围与内容。

;; 1. modified / added / renamed 的状态与行范围
(let* ((box (vector '()))
       (runner (make-runner
                 (list (list "git diff --name-status HEAD"
                         (string-append "M\t" sample "\n"
                           "A\tnew.ts\n"
                           "R100\told.ts\tnew-name.ts\n"
                         ) ;string-append
                         #f
                       ) ;list
                       (list (string-append "git diff --unified=0 --no-ext-diff HEAD -- "
                               sample
                             ) ;string-append
                         "@@ -1 +2,2 @@\n@@ -10 +20 @@\n"
                         #f
                       ) ;list
                       (list "git diff --unified=0 --no-ext-diff HEAD -- new-name.ts"
                         "@@ -1 +5,2 @@\n"
                         #f
                       ) ;list
                 ) ;list
                 box
               ) ;make-runner
       ) ;runner
       (res (pi-collect-simplify-files :runner runner))
       (files (cdr (assoc 'files res)))
      ) ;
  (check (field 'source res) => "diff")
  (check (length files) => 3)
  (check (field 'status (list-ref files 0)) => "modified")
  (check (field 'ranges (list-ref files 0))
    =>
    '(((start . 2) (end . 3)) ((start . 20) (end . 20)))
  ) ;check
  (check (field 'ranges-available? (list-ref files 0)) => #t)
  (check-true (string-contains? (field 'content (list-ref files 0)) "let t = 0"))
  (check (field 'status (list-ref files 1)) => "added")
  (check (field 'ranges (list-ref files 1)) => #f)
  (check (field 'ranges-available? (list-ref files 1)) => #t)
  (check (field 'status (list-ref files 2)) => "renamed")
  (check (field 'ranges (list-ref files 2)) => '(((start . 5) (end . 6))))
  (check (field 'path (list-ref files 2)) => "new-name.ts")
) ;let*

;; 2. 回退到 HEAD~1 时，行范围命令同样使用 HEAD~1（忽略 --staged）
(let* ((box (vector '()))
       (runner (make-runner
                 (list (list "git diff --name-status HEAD" "" #t)
                       (list "git diff --name-status HEAD~1" "M\ta.ts\n" #f)
                       (list "git diff --unified=0 --no-ext-diff HEAD~1 -- a.ts"
                         "@@ -1 +3,4 @@\n"
                         #f
                       ) ;list
                 ) ;list
                 box
               ) ;make-runner
       ) ;runner
       (res (pi-collect-simplify-files :runner runner :staged #t))
       (files (cdr (assoc 'files res)))
      ) ;
  (check (field 'source res) => "fallback")
  (check (field 'ranges (car files)) => '(((start . 3) (end . 6))))
) ;let*

;; 3. diff 失败时 ranges 为 #f 且 ranges-available? 为 #f
(let* ((box (vector '()))
       (runner (make-runner
                 (list (list "git diff --name-status HEAD" "M\ta.ts\n" #f)
                       (list "git diff --unified=0 --no-ext-diff HEAD -- a.ts"
                         "fatal: bad revision"
                         #t
                       ) ;list
                 ) ;list
                 box
               ) ;make-runner
       ) ;runner
       (res (pi-collect-simplify-files :runner runner))
       (file (car (cdr (assoc 'files res))))
      ) ;
  (check (field 'ranges file) => #f)
  (check (field 'ranges-available? file) => #f)
) ;let*

;; 4. 显式文件列表：状态视为 modified，仍按 ref 抓取行范围
(let* ((box (vector '()))
       (runner (make-runner
                 (list (list "git diff --unified=0 --no-ext-diff HEAD -- a.ts"
                         "@@ -1 +8 @@\n"
                         #f
                       ) ;list
                 ) ;list
                 box
               ) ;make-runner
       ) ;runner
       (res (pi-collect-simplify-files :runner runner :files '("a.ts")))
       (files (cdr (assoc 'files res)))
      ) ;
  (check (field 'source res) => "explicit")
  (check (length files) => 1)
  (check (field 'status (car files)) => "modified")
  (check (field 'ranges (car files)) => '(((start . 8) (end . 8))))
  (check (length (vector-ref box 0)) => 1)
) ;let*

;; 5. 超过上限时沿用 044 的提示
(let* ((many (apply string-append
               (map (lambda (i)
                      (string-append "M\tf" (number->string i) ".ts\n")
                    ) ;lambda
                 '(1 2 3 4 5 6 7 8 9 10 11 12)
               ) ;map
             ) ;apply
       ) ;many
       (runner (make-runner
                 (list (list "git diff --name-status HEAD" many #f))
                 (vector '())
               ) ;make-runner
       ) ;runner
       (res (pi-collect-simplify-files :runner runner))
      ) ;
  (check (length (cdr (assoc 'files res))) => 10)
  (check (field 'message res) => "Reviewing first 10 of 12 changed files.")
) ;let*

(check-report)
