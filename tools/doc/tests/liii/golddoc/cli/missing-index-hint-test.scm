;; 添加 tools/doc 到 load path，以便导入 (liii golddoc)
(set! *load-path* (append (list "tools/doc" ".") *load-path*))

(import (liii check) (liii golddoc) (liii list) (liii os) (liii path) (liii string) (liii sys))

(check-set-mode! 'report-failed)

;; gf doc 缺少函数索引时的提示
;; 当单参数查询同时可能是函数名时，如果当前测试根目录下没有
;; `function-library-index.json`，应提示执行 `gf doc --build-json`，
;; 而不是误报 library not found。

(define (run-shell-command command)
  (os-call (string-append "sh -c \"" command "\""))
) ;define

(define (unique-strings items)
  (let loop
    ((rest items) (acc '()))
    (cond ((null? rest) (reverse acc))
          ((member (car rest) acc) (loop (cdr rest) acc))
          (else (loop (cdr rest) (cons (car rest) acc)))
    ) ;cond
  ) ;let
) ;define

(define (collect-existing-index-paths)
  (let* ((repo-root (path-parent (path-parent (executable))))
         (candidates (append (find-function-index-paths)
                       (list (path-join "tests" "function-library-index.json")
                         (path-join ".." ".." "tests" "function-library-index.json")
                         (path-join "tools" "doc" "tests" "function-library-index.json")
                         (path-join repo-root "tests" "function-library-index.json")
                         (path-join repo-root "tools" "doc" "tests" "function-library-index.json")
                       ) ;list
                     ) ;append
         ) ;candidates
         (resolved-paths (map (lambda (p) (path->string (path-resolve p)))
                           (filter path-file? candidates)
                         ) ;map
         ) ;resolved-paths
        ) ;
    (unique-strings resolved-paths)
  ) ;let*
) ;define

(when (not (os-windows?))
  (let* ((index-paths (collect-existing-index-paths))
         (saved-entries (map (lambda (p) (cons p (path-read-text p))) index-paths))
         (output-path (path-join (path-temp-dir)
                        (string-append "golddoc-missing-index-" (number->string (getpid)) ".log")
                      ) ;path-join
         ) ;output-path
        ) ;
    (path-unlink output-path #t)
    (dynamic-wind (lambda ()
                    (for-each (lambda (p) (path-unlink p #t)) index-paths)
                  ) ;lambda
      (lambda ()
        (run-shell-command (string-append (executable)
                             " doc 'alist->fxmapping/combinator' > "
                             (path->string output-path)
                             " 2>&1"
                           ) ;string-append
        ) ;run-shell-command
        (let ((output (path-read-text output-path)))
          (check-true (string-contains? output
                        "Error: function index not found for query: alist->fxmapping/combinator"
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? output
                        "Hint: run `gf doc --build-json` to build function index."
                      ) ;string-contains?
          ) ;check-true
        ) ;let
      ) ;lambda
      (lambda ()
        (path-unlink output-path #t)
        (for-each (lambda (entry)
                    (path-write-text (car entry) (cdr entry))
                  ) ;lambda
          saved-entries
        ) ;for-each
      ) ;lambda
    ) ;dynamic-wind
  ) ;let*
) ;when

(check-report)
