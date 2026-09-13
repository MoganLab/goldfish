;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii goldtest)
  (import (scheme base)
    (scheme process-context)
    (liii sort)
    (except (liii list) remove)
    (liii string)
    (liii argparse)
    (liii goldtool-changed)
    (liii os)
    (liii path)
    (liii sys)
  ) ;import
  (export parse-test-args
    parse-test-changed-since
    filter-test-files
    filter-changed-test-files
    find-test-files
    failed-test-files
    split-tests-target
    run-goldtest
    main
  ) ;export
  (begin

    (define ESC (string #\escape #\[))

    (define (color code)
      (string-append ESC (number->string code) "m")
    ) ;define

    (define GREEN (color 32))
    (define RED (color 31))
    (define YELLOW (color 33))
    (define RESET (color 0))

    ;; Opt-in per-file timing (GOLDFISH_TEST_TIMING=1): record each worker's
    ;; wall milliseconds and report the distribution with the summary.
    ;; Default off: output and .code file formats stay exactly as before.
    (define timing-enabled?
      (let ((v (get-environment-variable "GOLDFISH_TEST_TIMING")))
        (and v (not (member v '("0" "no" "false" "off"))) #t)))

    (define (now-ms)
      ;; GNU date nanoseconds (Linux CI/dev); #f when unavailable.
      (let ((tmp (test-path-join (os-temp-dir)
                   (string-append "gf-test-clock-"
                                  (number->string (getpid)) ".txt"))))
        (shell-command (string-append "date +%s%N > " tmp " 2>&1"))
        (let ((ms (if (file-exists? tmp)
                    (let ((p (open-input-file tmp)))
                      (let ((v (string->number (read-line p))))
                        (close-input-port p)
                        (if (and v (integer? v) (>= v 0))
                          (quotient v 1000000)
                          #f)))
                    #f)))
          (when (file-exists? tmp) (remove tmp))
          ms)))

    (define *test-timings* '())

    (define (record-timing! f ms)
      (when (and timing-enabled? ms)
        (set! *test-timings* (cons (cons f ms) *test-timings*))))

    (define (parse-code-line line)
      ;; "exit" or "exit ms" -> (exit . ms-or-#f).
      (let ((n (string-length line)))
        (let loop ((i 0))
          (if (>= i n)
            (cons (string->number line) #f)
            (if (char=? (string-ref line i) #\space)
              (cons (string->number (substring line 0 i))
                    (string->number (substring line (+ i 1) n)))
              (loop (+ i 1)))))))

    (define (test-path-join . parts)
      ;; Normalize dotted-rest invocation for apply-style calls
      (let* ((parts (if (and (pair? parts) (pair? (car parts)) (string? (caar parts)))
                      (if (null? (cdr parts)) (car parts)
                          (if (let loop ((lst parts)) (if (null? lst) #t (and (pair? (car lst)) (string? (caar lst)) (null? (cdar lst)) (loop (cdr lst))))) (map car parts) parts))
                      parts))
             (sep (string (os-sep))))
        (let loop
          ((result "") (rest parts))
          (if (null? rest)
            result
            (let ((part (car rest)))
              (if (string-null? result)
                (loop part (cdr rest))
                (loop (string-append result sep part) (cdr rest))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (find-test-files dir)
      (if (not (path-dir? dir))
        '()
        (let ((entries (vector->list (listdir dir))))
          (let loop ((es entries) (acc '()))
            (if (null? es)
              acc
              (let* ((entry (car es))
                     (full-path (test-path-join dir entry))
                     (next-acc
                       (cond ((path-dir? full-path)
                              (append acc (find-test-files full-path)))
                             ((and (path-file? full-path) (string-ends? entry "-test.scm"))
                              (cons full-path acc))
                             (else acc))))
                (loop (cdr es) next-acc)))))))

    (define (goldfish-cmd)
      (string-append (executable) " -m liii ")
    ) ;define

    (define (worker-extra-path-args)
      ;; Test workers (`gf -m liii <file>`) resolve tool libraries through
      ;; the process working directory.  After switching into tools/<name>/,
      ;; the sibling tools/common/ (shared by the tool dispatch for
      ;; (liii goldtool-changed) etc.) is invisible there, so append it by
      ;; absolute path.  Outside a tool directory the candidate does not
      ;; exist and nothing is added.
      (let ((common-dir (test-path-join (getcwd) ".." "common")))
        (if (path-dir? common-dir)
          (string-append "-A " (shell-quote common-dir) " ")
          "")
      ) ;let
    ) ;define

    (define (run-test-file test-file)
      (let ((cmd (string-append (goldfish-cmd)
                                (worker-extra-path-args)
                                test-file)))
        (display "----------->")
        (newline)
        (display cmd)
        (newline)
        (let* ((t0 (and timing-enabled? (now-ms)))
               (result (os-call cmd))
               (t1 (and timing-enabled? (now-ms))))
          (record-timing! test-file (and t0 t1 (- t1 t0)))
          (cons test-file result)
        ) ;let
      ) ;let
    ) ;define

    (define (shell-quote s)
      (string-append "'" (string-replace s "'" "'\\''") "'")
    ) ;define

    (define (read-all-string file)
      (let ((p (open-input-file file)))
        (let loop ((acc '()))
          (let ((c (read-char p)))
            (if (eof-object? c)
              (begin (close-input-port p) (list->string (reverse acc)))
              (loop (cons c acc))
            ) ;if
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    (define (split-list lst n)
      (let loop ((l lst) (i 0) (acc '()))
        (if (or (null? l) (= i n))
          (values (reverse acc) l)
          (loop (cdr l) (+ i 1) (cons (car l) acc))
        ) ;if
      ) ;let
    ) ;define

    (define (detect-cpu-count)
      (if (os-windows?)
        1
        (let* ((tmp (test-path-join (os-temp-dir)
                       (string-append "gf-test-nproc-" (number->string (getpid)) ".txt")))
               (cmd (string-append "nproc > " tmp " 2>&1")))
          (shell-command cmd)
          (let ((n (if (file-exists? tmp)
                     (let ((p (open-input-file tmp)))
                       (let ((v (string->number (read-line p))))
                         (close-input-port p)
                         v
                       ) ;let
                     ) ;let
                     #f
                   ) ;if
                 ) ;n
               ) ;
            (when (file-exists? tmp) (remove tmp))
            (if (and n (integer? n) (> n 0)) n 1)
          ) ;let
        ) ;let*
      ) ;if
    ) ;define

    (define (run-test-batch files)
      ;; Run `files' concurrently: one shell script backgrounds every run,
      ;; capturing stdout+stderr and the exit status into temp files, then
      ;; waits for all of them.  The output of a failed file is printed (in
      ;; file order) so failures stay debuggable.
      ;; Workers share the cache directory and write through it: entry
      ;; writes are atomic (pid-suffixed tmp + rename in gfo-write!), so
      ;; concurrent batches progressively warm the cache instead of each
      ;; re-expanding the world.  Honor an explicit user-provided
      ;; GOLDFISH_CACHE_READONLY, but never force it here.
      (let* ((tag (number->string (getpid)))
             (specs (let loop ((fs files) (i 0) (acc '()))
                      (if (null? fs)
                        (reverse acc)
                        (let* ((f (car fs))
                               (out (test-path-join (os-temp-dir)
                                      (string-append "gf-test-" tag "-" (number->string i) ".out")))
                               (code (test-path-join (os-temp-dir)
                                       (string-append "gf-test-" tag "-" (number->string i) ".code"))))
                          (loop (cdr fs) (+ i 1)
                                (cons (list f out code) acc))))))
              (script (string-append
                        (string-join
                           (map (lambda (s)
                                 (let ((core (string-append (shell-quote (executable))
                                                           " -m liii " (worker-extra-path-args)
                                                           (shell-quote (car s))
                                                           " > " (shell-quote (cadr s))
                                                           " 2>&1")))
                                   (if timing-enabled?
                                     (string-append "(t0=$(date +%s%N); " core
                                                    "; c=$?; t1=$(date +%s%N); echo $c $(( (t1-t0)/1000000 ))"
                                                    " > " (shell-quote (caddr s)) ") &")
                                     (string-append "(" core
                                                    "; echo $? > " (shell-quote (caddr s)) ") &"))))
                              specs)
                         " ")
                       " wait"))
             (script-file (test-path-join (os-temp-dir)
                            (string-append "gf-test-" tag "-batch.sh"))))
        (call-with-output-file script-file
          (lambda (p) (display script p)))
        (os-call (string-append "sh " (shell-quote script-file)))
        (let ((results
                (map (lambda (s)
                       (let* ((f (car s)) (out (cadr s)) (code (caddr s))
                              (parsed (if (file-exists? code)
                                        (let ((p (open-input-file code)))
                                          (let ((v (parse-code-line (read-line p))))
                                            (close-input-port p)
                                            v))
                                        #f))
                              (n (and parsed (car parsed)))
                             ) ;
                         (record-timing! f (and parsed (cdr parsed)))
                         (when (and n (not (zero? n)))
                           (newline)
                           (display "----------->")
                           (newline)
                           (display f)
                           (newline)
                           (if (file-exists? out)
                             (begin (display (read-all-string out)) (newline)))
                         ) ;when
                         (when (file-exists? out) (remove out))
                         (when (file-exists? code) (remove code))
                         (cons f (if n n -1))
                       ) ;let*
                     ) ;lambda
                     specs
                   ) ;map
                 ) ;results
               ) ;
          (when (file-exists? script-file) (remove script-file))
          results
        ) ;let
      ) ;let*
    ) ;define

    (define (run-test-files test-files jobs)
      ;; jobs<=1 keeps the original serial behavior (inline output);
      ;; jobs>1 runs batches of `jobs' files concurrently.
      (if (<= jobs 1)
        (map (lambda (f) (run-test-file f)) test-files)
        (if (and workers-enabled? (find-worker-program))
          (run-test-worker-batches test-files jobs)
          (let loop ((files test-files) (acc '()))
            (if (null? files)
              (reverse acc)
              (let-values (((head tail) (split-list files jobs)))
                (loop tail (append (reverse (run-test-batch head)) acc))
              ) ;let-values
            ) ;if
          ) ;let
        ) ;if
      ) ;if
    ) ;define

    (define workers-enabled?
      (let ((v (get-environment-variable "GOLDFISH_TEST_WORKERS")))
        (and v (not (member v '("0" "no" "false" "off"))) #t)))

    (define worker-chunk-size 32)

    ;; Files that must run one-process-per-file (matched by suffix):
    ;; (exit 0) network skip-guards would kill a shared worker, and a few
    ;; tests assert fresh-process expander/loader state.
    (define worker-isolated-files
      '("tests/liii/http/http-wait-all-test.scm"
        "tests/liii/http/http-post-test.scm"
        "tests/liii/http/http-poll-test.scm"
        "tests/liii/http/http-ok-p-test.scm"
        "tests/liii/http/http-head-test.scm"
        "tests/liii/http/http-get-test.scm"
        "tests/liii/http/http-async-post-test.scm"
        "tests/liii/http/http-async-head-test.scm"
        "tests/liii/http/http-async-get-test.scm"
        ;; Asserts fresh-process expander surface.
        "tests/expander/internal-surface-test.scm"
        ;; Stale import views pinned from partial sources (audit's
        ;; deliberate-failure loads); needs the view-lifetime fix.
        "tests/expander/lib-cache-test.scm"
        "tests/expander/lib-cache-all-libs-test.scm"
        ;; Order/layout-sensitive in shared processes (proven pairs).
        "tests/liii/bag/bag-replace-test.scm"
        "tests/liii/base/copy-test.scm"
        "tests/liii/queue/list-queue-first-last-test.scm"
        "tests/liii/vector/vector-map-bang-test.scm"
        "tests/liii/vector/vector-set-bang-test.scm"
        ;; Layout-sensitive under worker heap reuse (flake in chunks,
        ;; stable isolated).
        "tests/scheme/base/append-test.scm"
        "tests/scheme/base/assq-test.scm"
        "tests/scheme/base/bytevector-p-test.scm"
        "tests/scheme/base/list-p-test.scm"
        "tests/scheme/base/list-tail-test.scm"
        ;; Worker-context check failures (mechanism TBD).
        "tests/compiler/syntax-ir-test.scm"
        "tests/goldfish/liii/project-test.scm"
        "tests/liii/expander/expander-test.scm"
        "tests/srfi/srfi-78-test.scm"))

    (define (worker-isolated? f)
      (let loop ((ls worker-isolated-files))
        (if (null? ls)
          #f
          (let ((e (car ls)))
            (if (and (>= (string-length f) (string-length e))
                     (equal? (substring f (- (string-length f)
                                             (string-length e)))
                             e))
              #t
              (loop (cdr ls)))))))

    (define (find-worker-program)
      (let loop ((cands '("tools/test/liii/worker.scm"
                          "../test/liii/worker.scm")))
        (if (null? cands)
          #f
          (if (file-exists? (car cands))
            (car cands)
            (loop (cdr cands))))))

    (define (split-spaces s)
      (let ((n (string-length s)))
        (let loop ((i 0) (start 0) (acc '()))
          (if (>= i n)
            (reverse (if (> i start) (cons (substring s start i) acc) acc))
            (if (char=? (string-ref s i) #\space)
              (loop (+ i 1) (+ i 1)
                    (if (> i start) (cons (substring s start i) acc) acc))
              (loop (+ i 1) start acc))))))

    (define (read-all-lines path)
      (if (not (file-exists? path))
        '()
        (let ((p (open-input-file path)))
          (let loop ((acc '()))
            (let ((line (read-line p)))
              (if (eof-object? line)
                (begin (close-input-port p) (reverse acc))
                (loop (cons line acc))))))))

    (define (parse-worker-line line)
      ;; ";;;WORKER <file> <code> <ms>" -> (file code ms) or #f.
      (if (and (>= (string-length line) 10)
               (equal? (substring line 0 10) ";;;WORKER "))
        (let ((toks (split-spaces (substring line 10 (string-length line)))))
          (if (and (= (length toks) 3)
                   (string->number (cadr toks))
                   (string->number (caddr toks)))
            (list (car toks)
                  (string->number (cadr toks))
                  (string->number (caddr toks)))
            #f))
        #f))

    (define (worker-done-line? line)
      (and (>= (string-length line) 15)
           (equal? (substring line 0 15) ";;;WORKER-DONE ")))

    (define (chunk-list lst n)
      (let loop ((l lst) (acc '()))
        (if (null? l)
          (reverse acc)
          (let-values (((head tail) (split-list l n)))
            (loop tail (cons head acc))))))

    (define (worker-chunk-command files worker out)
      (string-append "GOLDFISH_CHECK_NO_EXIT=1 "
                     (shell-quote (executable))
                     " -m liii " (worker-extra-path-args)
                     (shell-quote worker)
                     " -- "
                     (string-join (map shell-quote files) " ")
                     " > " (shell-quote out) " 2>&1"))

    (define (parse-worker-out out)
      ;; Alist file -> (code . ms) from ;;;WORKER lines; #t done?
      (let ((lines (read-all-lines out)))
        (let loop ((ls lines) (acc '()) (done #f))
          (if (null? ls)
            (cons acc done)
            (let ((e (parse-worker-line (car ls))))
              (loop (cdr ls)
                    (if e (cons (cons (car e) (cons (cadr e) (caddr e))) acc)
                          acc)
                    (or done (worker-done-line? (car ls)))))))))

    (define (run-worker-wave chunks worker tag idx0)
      ;; One script, one background worker per chunk, then collect.
      (let* ((specs (let loop ((cs chunks) (i idx0) (acc '()))
                      (if (null? cs)
                        (reverse acc)
                        (let ((out (test-path-join
                                     (os-temp-dir)
                                     (string-append "gf-worker-" tag "-"
                                                    (number->string i) ".out"))))
                          (loop (cdr cs) (+ i 1)
                                (cons (list (car cs) out) acc))))))
             (script (string-append
                       (string-join
                         (map (lambda (s)
                                (string-append "("
                                               (worker-chunk-command (car s) worker (cadr s))
                                               ") &"))
                              specs)
                         " ")
                       " wait"))
             (script-file (test-path-join (os-temp-dir)
                            (string-append "gf-worker-" tag "-wave.sh"))))
        (call-with-output-file script-file
          (lambda (p) (display script p)))
        (os-call (string-append "sh " (shell-quote script-file)))
        (when (file-exists? script-file) (remove script-file))
        (let ((results
                (map (lambda (s)
                       (let* ((files (car s)) (out (cadr s))
                              (parsed (parse-worker-out out))
                              (table (car parsed))
                              (done? (cdr parsed)))
                         (let ((rs
                                 (map (lambda (f)
                                        (let ((e (assoc f table)))
                                          (if (and done? e)
                                            (begin (record-timing! f (cddr e))
                                                   (cons f (cadr e)))
                                            ;; Worker never reported it:
                                            ;; fall back to an isolated run.
                                            (run-test-file f))))
                                      files)))
                           (when (let ((bad (filter (lambda (r)
                                                      (not (zero? (cdr r))))
                                                    rs)))
                                   (and (pair? bad) (file-exists? out)))
                             (newline)
                             (display "----------->")
                             (newline)
                             (display (string-append "worker chunk: "
                                                     (string-join files " ")))
                             (newline)
                             (display (read-all-string out))
                             (newline))
                           (when (file-exists? out) (remove out))
                           rs)))
                     specs)))
          (apply append results))))

    (define (run-test-worker-batches test-files jobs)
      (let* ((tag (number->string (getpid)))
             (worker (find-worker-program))
             (isolated (filter worker-isolated? test-files))
             (chunked (filter (lambda (f) (not (worker-isolated? f)))
                              test-files))
             (chunks (chunk-list chunked worker-chunk-size)))
        (append
          ;; Isolated files keep the one-process-per-file path.
          (let loop ((files isolated) (acc '()))
            (if (null? files)
              (reverse acc)
              (let-values (((head tail) (split-list files jobs)))
                (loop tail (append (reverse (run-test-batch head)) acc)))))
          ;; Worker chunks ride waves of `jobs' concurrent workers.
          (let loop ((cs chunks) (i 0) (acc '()))
            (if (null? cs)
              (reverse acc)
              (let-values (((head tail) (split-list cs jobs)))
                (loop tail (+ i (length head))
                      (append (reverse (run-worker-wave head worker tag i))
                              acc))))))))

    (define (failed-test-files test-results)
      (map car
        (filter (lambda (test-result) (not (zero? (cdr test-result)))) test-results)
      ) ;map
    ) ;define

    (define (display-summary test-results)
      (let ((total (length test-results))
            (passed (count (lambda (x) (zero? (cdr x))) test-results))
            (failed-files (failed-test-files test-results))
            (failed (- (length test-results) (count (lambda (x) (zero? (cdr x))) test-results))
            ) ;failed
           ) ;
        (newline)
        (display "=== Test Summary ===")
        (newline)
        (newline)
        (for-each (lambda (test-result)
                    (let ((test-file (car test-result)) (exit-code (cdr test-result)))
                      (display (string-append "  " test-file " ... "))
                      (if (zero? exit-code)
                        (display (string-append GREEN "PASS" RESET))
                        (display (string-append RED "FAIL" RESET))
                      ) ;if
                      (newline)
                    ) ;let
                  ) ;lambda
          test-results
        ) ;for-each
        (newline)
        (display "=== Summary ===")
        (newline)
        (display (string-append "  Total:  " (number->string total)))
        (newline)
        (display (string-append "  " GREEN "Passed: " (number->string passed) RESET))
        (newline)
        (when (> failed 0)
          (display (string-append "  " RED "Failed: " (number->string failed) RESET))
          (newline)
          (display "  Failed Test Files:")
          (newline)
          (for-each (lambda (test-file) (display (string-append "    " test-file)) (newline))
            failed-files
          ) ;for-each
        ) ;when
        (when (and timing-enabled? (pair? *test-timings*))
          (let* ((ms-list (map cdr *test-timings*))
                 (total-ms (apply + ms-list))
                 (sorted (list-sort (lambda (a b) (> (cdr a) (cdr b)))
                                    *test-timings*))
                 (buckets (let loop ((ls ms-list)
                                     (b100 0) (b500 0) (b1000 0) (b5000 0) (bign 0))
                            (if (null? ls)
                              (list b100 b500 b1000 b5000 bign)
                              (let ((m (car ls)))
                                (loop (cdr ls)
                                      (if (< m 100) (+ b100 1) b100)
                                      (if (and (>= m 100) (< m 500)) (+ b500 1) b500)
                                      (if (and (>= m 500) (< m 1000)) (+ b1000 1) b1000)
                                      (if (and (>= m 1000) (< m 5000)) (+ b5000 1) b5000)
                                      (if (>= m 5000) (+ bign 1) bign))))))
                 (top (let loop ((ls sorted) (i 0) (acc '()))
                        (if (or (null? ls) (>= i 10))
                          (reverse acc)
                          (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))
            (newline)
            (display "=== Timing (ms) ===")
            (newline)
            (display (string-append "  Files: " (number->string (length ms-list))
                                   ", total CPU: " (number->string total-ms)
                                   ", avg: " (number->string (quotient total-ms (length ms-list)))
                                   ", max: " (number->string (cdar sorted))))
            (newline)
            (display (string-append "  Buckets(ms): <100:" (number->string (car buckets))
                                   " 100-500:" (number->string (cadr buckets))
                                   " 500-1000:" (number->string (caddr buckets))
                                   " 1000-5000:" (number->string (cadddr buckets))
                                   " >=5000:" (number->string (car (cddddr buckets)))))
            (newline)
            (display "  Slowest:")
            (newline)
            (for-each (lambda (e)
                        (display (string-append "    " (number->string (cdr e))
                                               " " (car e)))
                        (newline))
              top)
          ) ;let
        ) ;when
        (newline)
        failed
      ) ;let
    ) ;define

    (define (make-test-arg-parser)
      (let ((parser (make-argument-parser '((command . "test")
                                            (skip-value-options "-m" "--mode")
                                            (skip-prefix-options "-m="
                                              "--mode=")
                                            (unknown-options . positional))
                    ) ;make-argument-parser
            ) ;parser
           ) ;
        (parser :add-argument '((name . "changed-since") (type . string)))
        (parser :add-argument '((name . "jobs") (short . "j") (type . string)))
        (parser :add-argument '((name . "all") (action . store-true)))
        (parser :add-argument '((name . "help")
                                (short . "h")
                                (action . store-true)))
        parser
      ) ;let
    ) ;define

    (define (classify-test-arg arg)
      (cond
        ;; 包含路径分隔符的路径 (/ 或 Windows 的 \)
        ((or (string-contains arg "/") (and (os-windows?) (string-contains arg "\\")))
         (let ((abs-path (if (path-absolute? arg) arg (path-join (getcwd) arg))))
           (cond ((path-file? abs-path) (cons 'file arg))
                 ((path-dir? abs-path) (cons 'dir arg))
                 (else (cons 'pattern arg))
           ) ;cond
         ) ;let
        ) ;
        ;; 以 .scm 结尾的文件名
        ((string-ends? arg ".scm") (cons 'filename arg))
        ;; 其他视为模糊匹配模式
        (else (cons 'pattern arg))
      ) ;cond
    ) ;define

    (define (parse-test-args args)
      ;; 解析 test 命令的参数
      ;; 规则：
      ;; 1. 如果参数包含 /，视为路径处理
      ;;    - 如果是存在的文件，直接返回该文件
      ;;    - 如果是存在的目录，返回该目录用于后续查找
      ;; 2. 如果参数以 .scm 结尾但不是路径，按文件名匹配
      ;; 3. 其他情况，按模糊匹配（路径中包含该字符串）
      ;; 返回值: (type . value)
      ;;   type 可以是: 'file, 'dir, 'filename, 'pattern, #f
      ;; args 的第一个元素是可执行文件路径，需要跳过
      (let ((parser (make-test-arg-parser)))
        (parser :parse-argv args)
        (let ((positionals (parser :positionals)))
          (if (null? positionals) (cons #f #f) (classify-test-arg (car positionals)))
        ) ;let
      ) ;let
    ) ;define

    (define (parse-test-changed-since args)
      (let ((parser (make-test-arg-parser)))
        (parser :parse-argv args)
        (parser 'changed-since)
      ) ;let
    ) ;define

    (define (parse-test-jobs args)
      (let ((parser (make-test-arg-parser)))
        (parser :parse-argv args)
        (let ((j (parser 'jobs)))
          (if (and j (string? j))
            (let ((n (string->number j)))
              (if (and n (integer? n) (> n 0)) n #f)
            ) ;let
            #f
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (filter-test-files test-files arg-type arg-value)
      ;; 根据参数类型过滤测试文件
      (case arg-type
       ((file)
        ;; 直接返回单个文件（已经在 parse-test-args 中验证存在）
        (list arg-value)
       ) ;
       ((dir)
        ;; 返回该目录下的所有测试文件
        ;; 在 Windows 上，将用户输入的正斜杠转换为反斜杠以匹配文件路径
        (let ((dir-pattern (if (os-windows?) (string-replace arg-value "/" "\\") arg-value)))
          (filter (lambda (file) (string-starts? file dir-pattern)) test-files)
        ) ;let
       ) ;
       ((filename)
        ;; 精确匹配文件名
        (filter (lambda (file) (string=? (path-name file) arg-value)) test-files)
       ) ;
       ((pattern)
        ;; 模糊匹配路径
        (filter (lambda (file) (string-contains file arg-value)) test-files)
       ) ;
       (else
         ;; 无参数，返回所有文件
         test-files
       ) ;else
      ) ;case
    ) ;define

    (define (normalize-test-file-path file)
      (if (os-windows?) (string-replace file "\\" "/") file)
    ) ;define

    (define (filter-changed-test-files test-files since)
      (let ((changed-files (changed-scheme-files-since since)))
        (filter (lambda (file) (member (normalize-test-file-path file) changed-files))
          test-files
        ) ;filter
      ) ;let
    ) ;define

    (define (display-filter-info arg-type arg-value)
      ;; 显示过滤信息
      (case arg-type
       ((file) (display (string-append "Running test file: " arg-value)) (newline))
       ((dir)
        (display (string-append "Running tests in directory: " arg-value))
        (newline)
       ) ;
       ((filename)
        (display (string-append "Running tests with file name: " arg-value))
        (newline)
       ) ;
       ((pattern)
        (display (string-append "Running tests matching pattern: " arg-value))
        (newline)
       ) ;
      ) ;case
    ) ;define

    (define (split-tests-target target)
      ;; 将 .../tests 或 .../tests/... 路径拆成父目录和相对 tests 路径
      (let ((marker-length 6))
        (let loop
          ((i 0))
          (cond ((> i (- (string-length target) marker-length)) #f)
                ((and (or (string=? (substring target i (+ i marker-length)) "/tests")
                        (string=? (substring target i (+ i marker-length)) "\\tests")
                      ) ;or
                   (or (= (+ i marker-length) (string-length target))
                     (char=? (string-ref target (+ i marker-length)) #\/)
                     (char=? (string-ref target (+ i marker-length)) #\\)
                   ) ;or
                 ) ;and
                 (let ((parent (substring target 0 i)) (next-pos (+ i marker-length)))
                   (if (> (string-length parent) 0)
                     (let ((tests-path (substring target (+ i 1))))
                       (if (= next-pos (string-length target))
                         (cons parent (string-append tests-path (string (string-ref target i))))
                         (cons parent tests-path)
                       ) ;if
                     ) ;let
                     #f
                   ) ;if
                 ) ;let
                ) ;
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (check-and-switch-to-target args)
      ;; 检查是否需要切换到 target 目录
      ;; 规则：
      ;; 1. 如果第一个非选项参数是目录，且该目录下有 tests 子目录，则切换
      ;; 2. 如果参数路径以 /tests 或 /tests/ 结尾，提取父目录作为 target 并切换
      ;; 返回切换后的新参数列表（如果切换了，需要去掉或修改 target 参数）
      (let loop
        ((remaining (cdr args)) (skip-next #f) (found-target #f))
        (cond
          ;; 没有更多参数
          ((null? remaining)
           (if found-target
             (let* ((target found-target) (tests-target (split-tests-target target)))
               (cond
                 ;; 情况 1: 路径包含 /tests/，按原有逻辑处理
                 (tests-target (let ((parent (car tests-target)) (tests-path (cdr tests-target)))
                                 (if (path-dir? parent)
                                   (begin
                                     (chdir parent)
                                     ;; 切换目录后，将参数改为 tests/...（相对路径）
                                     (cons (car args)
                                       (map (lambda (arg) (if (equal? arg target) tests-path arg)) (cdr args))
                                     ) ;cons
                                   ) ;begin
                                   args
                                 ) ;if
                               ) ;let
                 ) ;tests-target
                 ;; 情况 2: 目标是目录且包含 tests 子目录
                 ((and (path-dir? target) (path-dir? (test-path-join target "tests")))
                  (chdir target)
                  ;; 切换目录后，将参数中的 target 替换为 "tests"
                  (cons (car args)
                    (map (lambda (arg) (if (equal? arg target) "tests" arg)) (cdr args))
                  ) ;cons
                 ) ;
                 ;; 其他情况，不切换
                 (else args)
               ) ;cond
             ) ;let*
             args
           ) ;if
          ) ;
          ;; 跳过选项值
          (skip-next (loop (cdr remaining) #f found-target))
          ;; 跳过 test 命令本身
          ((equal? (car remaining) "test") (loop (cdr remaining) #f found-target))
          ;; 跳过 -m/--mode 及其值
          ((or (equal? (car remaining) "-m") (equal? (car remaining) "--mode"))
           (loop (if (null? (cdr remaining)) '() (cddr remaining)) #f found-target)
          ) ;
          ;; 跳过 -m=.../--mode=... 格式
          ((or (string-starts? (car remaining) "-m=")
             (string-starts? (car remaining) "--mode=")
           ) ;or
           (loop (cdr remaining) #f found-target)
          ) ;
          ;; 跳过 --changed-since 及其值
          ((equal? (car remaining) "--changed-since")
           (loop (if (null? (cdr remaining)) '() (cddr remaining)) #f found-target)
          ) ;
          ;; 跳过 --changed-since=... 格式
          ((string-starts? (car remaining) "--changed-since=")
           (loop (cdr remaining) #f found-target)
          ) ;
          ;; 跳过 --all
          ((equal? (car remaining) "--all") (loop (cdr remaining) #f found-target))
          ;; 找到 target（第一个非选项参数）
          ((not found-target) (loop (cdr remaining) #f (car remaining)))
          ;; 其他参数，继续
          (else (loop (cdr remaining) #f found-target))
        ) ;cond
      ) ;let
    ) ;define

    (define (shell-command command)
      ;; os-call does not perform shell redirection (wordexp + exec on
      ;; POSIX): `>` and `2>&1` would be passed as literal argv to the
      ;; program.  Run the command through `sh -c` so redirection works.
      (if (os-windows?)
        (os-call command)
        (os-call (string-append "sh -c '" (string-replace command "'" "'\\''") "'"))
      ) ;if
    ) ;define

    (define (git-current-branch)
      (let* ((tmp-file (test-path-join (os-temp-dir)
                         (string-append "gf-test-branch-" (number->string (getpid)) ".txt")
                       ) ;test-path-join
             ) ;tmp-file
             (cmd (string-append "git rev-parse --abbrev-ref HEAD > " tmp-file " 2>&1"))
             (exit-code (shell-command cmd))
            ) ;
        (if (zero? exit-code)
          (let* ((port (open-input-file tmp-file)) (branch (read-line port)))
            (close-input-port port)
            (remove tmp-file)
            branch
          ) ;let*
          (begin
            (when (file-exists? tmp-file)
              (remove tmp-file)
            ) ;when
            #f
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    (define (git-branch-exists? branch)
      (let* ((tmp-file (test-path-join (os-temp-dir)
                         (string-append "gf-branch-check-" (number->string (getpid)) ".txt")
                       ) ;test-path-join
             ) ;tmp-file
             (cmd (string-append "git rev-parse --verify " branch " > " tmp-file " 2>&1"))
             (exit-code (shell-command cmd))
            ) ;
        (when (file-exists? tmp-file)
          (remove tmp-file)
        ) ;when
        (zero? exit-code)
      ) ;let*
    ) ;define

    (define (parse-test-all args)
      (let ((parser (make-test-arg-parser)))
        (parser :parse-argv args)
        (parser 'all)
      ) ;let
    ) ;define

    (define (route-test-command args all-mode)
      (let ((branch (git-current-branch)) (exe (executable)))
        (cond ((and branch (not (string=? branch "main")) (git-branch-exists? "main"))
               (if all-mode
                 (begin
                   (display (string-append "[gf test] Not on main branch (currently on '"
                              branch
                              "'), running all tests (--all) in 2 phases: changed-since=main first, then the rest. Use `gf test --help` for details."
                            ) ;string-append
                   ) ;display
                   (newline)
                   (display (string-append "Phase 1: " exe " test --changed-since=main"))
                   (newline)
                   (display (string-append "Phase 2: " exe " test tests (remaining)"))
                   (newline)
                   (newline)
                   (cons "main" #t)
                 ) ;begin
                 (begin
                   (display (string-append "[gf test] Not on main branch (currently on '"
                              branch
                              "'), running changed tests since main. Use `gf test --help` for details."
                            ) ;string-append
                   ) ;display
                   (newline)
                   (display (string-append "Running: " exe " test --changed-since=main"))
                   (newline)
                   (newline)
                   (cons "main" #f)
                 ) ;begin
               ) ;if
              ) ;
              ((and branch (not (string=? branch "main")))
               (display "[gf test] Git repo has no main branch, running all tests. Use `gf test --help` for details."
               ) ;display
               (newline)
               (display (string-append "Running: " exe " test tests"))
               (newline)
               (newline)
               (cons #f #f)
              ) ;
              (branch (display "[gf test] On main branch, running all tests. Use `gf test --help` for details."
                      ) ;display
                (newline)
                (display (string-append "Running: " exe " test tests"))
                (newline)
                (newline)
                (cons #f #f)
              ) ;branch
              (else (display "[gf test] Not a git repository, running all tests. Use `gf test --help` for details."
                    ) ;display
                (newline)
                (display (string-append "Running: " exe " test tests"))
                (newline)
                (newline)
                (cons #f #f)
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (run-goldtest)
      (let* ((raw-args (command-line))
             (args (check-and-switch-to-target raw-args))
             (all-mode (parse-test-all args))
             (changed-since (parse-test-changed-since args))
             (parsed (parse-test-args args))
             (arg-type (car parsed))
             (arg-value (cdr parsed))
             ;; 智能路由：无显式参数时根据 git 状态决定
             (route-result (if (and (not arg-type) (not changed-since))
                             (route-test-command args all-mode)
                             #f
                           ) ;if
             ) ;route-result
             (final-changed-since (if route-result (car route-result) changed-since))
             (need-run-all (if route-result (cdr route-result) #f))
             (all-test-files (list-sort string<? (find-test-files "tests")))
             (filtered-test-files (filter-test-files all-test-files arg-type arg-value))
             ;; 如果指定了 changed-since，先过滤出变更的测试
             (changed-test-files (if final-changed-since
                                   (filter-changed-test-files filtered-test-files final-changed-since)
                                   '()
                                 ) ;if
             ) ;changed-test-files
             ;; 在 --all 模式下，把未变更的测试追加在后面
             (remaining-test-files (if need-run-all
                                     (filter (lambda (f) (not (member f changed-test-files))) filtered-test-files)
                                     '()
                                   ) ;if
             ) ;remaining-test-files
             (test-files (if need-run-all
                           (append changed-test-files remaining-test-files)
                           (if final-changed-since changed-test-files filtered-test-files)
                         ) ;if
             ) ;test-files
            ) ;
        (if (null? test-files)
          (begin
            (if final-changed-since
              (begin
                (display (string-append YELLOW "No test files changed since " final-changed-since RESET)
                ) ;display
                (newline)
              ) ;begin
              (if arg-value
                (begin
                  (display (string-append YELLOW "No test files matching " arg-value RESET))
                  (newline)
                ) ;begin
                (begin
                  (display (string-append YELLOW "No test files found in tests directory" RESET))
                  (newline)
                ) ;begin
              ) ;if
            ) ;if
            (exit 0)
          ) ;begin
          (begin
            (when arg-value
              (display-filter-info arg-type arg-value)
            ) ;when
            (let* ((jobs (or (parse-test-jobs args) (detect-cpu-count)))
                   (two-phases? (and need-run-all
                                      (pair? changed-test-files)
                                      (pair? remaining-test-files)))
                   (test-results
                     (if two-phases?
                       (append
                         (begin
                           (display (string-append "Phase 1/2: running "
                                                   (number->string (length changed-test-files))
                                                   " changed tests since "
                                                   final-changed-since))
                           (newline)
                           (run-test-files changed-test-files jobs))
                         (begin
                           (display (string-append "Phase 2/2: running "
                                                   (number->string (length remaining-test-files))
                                                   " remaining tests"))
                           (newline)
                           (run-test-files remaining-test-files jobs)))
                       (begin
                         (when final-changed-since
                           (display (string-append "Running changed tests since: "
                                                   final-changed-since))
                           (newline))
                         (run-test-files test-files jobs)))))
              (let ((failed (display-summary test-results)))
                (exit (if (> failed 0) -1 0))
              ) ;let
            ) ;let*
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    (define (show-help)
      ;; 显示帮助信息
      (display "gf test - Goldfish Scheme Test Runner")
      (newline)
      (newline)
      (display "Usage:")
      (newline)
      (display "  gf test [options] [PATH|PATTERN]")
      (newline)
      (newline)
      (display "Options:")
      (newline)
      (display "  --all                            Run all tests (greedy: changed first, then all)"
      ) ;display
      (newline)
      (display "  --changed-since REV              Run tests changed since REV")
      (newline)
      (display "  -j, --jobs N                     Run up to N test files concurrently (default: CPU count)")
      (newline)
      (display "                                  Use -j 1 for the original serial behavior")
      (newline)
      (newline)
      (display "Examples:")
      (newline)
      (display "  gf test                          Run tests (smart route based on git branch)"
      ) ;display
      (newline)
      (display "  gf test --all                    Run all tests")
      (newline)
      (display "  gf test tools/doc/tests/         Run tests in directory")
      (newline)
      (display "  gf test tests/liii/string/       Run tests in directory")
      (newline)
      (display "  gf test string-test.scm          Run specific test file")
      (newline)
      (display "  gf test string                   Run tests matching pattern")
      (newline)
      (display "  gf test --changed-since=HEAD     Run changed test files")
      (newline)
      (newline)
      (display "Note:")
      (newline)
      (display "  Smart routing: on non-main branch, gf test runs --changed-since=main"
      ) ;display
      (newline)
      (display "  If path contains /tests/, it will switch to parent directory")
      (newline)
      (display "  e.g., gf test tools/doc/tests/  =>  cd tools/doc && gf test tests/")
      (newline)
    ) ;define

    (define (test-help-requested? args)
      (let ((parser (make-test-arg-parser)))
        (parser :parse-argv args)
        (parser 'help)
      ) ;let
    ) ;define

    (define (main)
      ;; 程序入口点
      (let ((args (command-line)))
        (if (test-help-requested? args) (begin (show-help) (exit 0)) (run-goldtest))
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
