(define-library (liii goldfix)
  (export main fix-string repair-parentheses parentheses-balanced?)
  (import (liii base)
    (liii sys)
    (liii path)
    (liii argparse)
    (srfi srfi-13)
    (liii goldfix-repair)
    (liii goldfix-record)
  ) ;import

  (begin
    (define (display-help)
      (display "Usage: gf fix [options] [path]")
      (newline)
      (newline)
      (display "Options:")
      (newline)
      (display "  -h, --help       显示此帮助文档")
      (newline)
      (display "      --dry-run    预览模式（仅支持单个文件）")
      (newline)
      (newline)
      (display "Arguments:")
      (newline)
      (display "  path    要修正括号的文件或目录路径（可选）")
      (newline)
      (newline)
      (display "Examples:")
      (newline)
      (display "  gf fix                        显示此帮助文档")
      (newline)
      (display "  gf fix --help                 显示此帮助文档")
      (newline)
      (display "  gf fix file.scm               修正单个文件")
      (newline)
      (display "  gf fix --dry-run file.scm     预览修正结果")
      (newline)
      (display "  gf fix /path/to/dir           递归修正目录下所有 .scm 文件"
      ) ;display
      (newline)
    ) ;define

    (define (display-diagnostics diagnostics)
      (let loop
        ((rest diagnostics))
        (if (not (null? rest))
          (begin
            (display "诊断: ")
            (write (car rest))
            (newline)
            (loop (cdr rest))
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    (define (repair-source source)
      (call-with-values (lambda () (repair-parentheses source))
        (lambda (repaired report)
          (if (repair-report-ok? report)
            repaired
            (begin
              (display-diagnostics (repair-report-diagnostics report))
              repaired
            ) ;begin
          ) ;if
        ) ;lambda
      ) ;call-with-values
    ) ;define

    (define (fix-file-dry-run path-str)
      (let ((source (path-read-text (path path-str))))
        (display (repair-source source))
        (newline)
      ) ;let
    ) ;define

    (define (fix-file path-str)
      (let* ((p (path path-str))
             (source (path-read-text p))
             (repaired (repair-source source))
            ) ;
        (if (string=? source repaired) #f (begin (path-write-text p repaired) #t))
      ) ;let*
    ) ;define

    (define (fix-directory dir-path)
      (let ((entries (path-list-path (path dir-path))))
        (let loop
          ((i 0) (total 0) (updated 0))
          (if (>= i (vector-length entries))
            (values total updated)
            (let ((entry (vector-ref entries i)))
              (cond ((path-file? entry)
                     (let ((entry-str (path->string entry)))
                       (if (string-suffix? ".scm" entry-str)
                         (begin
                           (display (string-append "Processing: " entry-str))
                           (newline)
                           (if (fix-file entry-str)
                             (begin
                               (display (string-append "  Updated: " entry-str))
                               (newline)
                               (loop (+ i 1) (+ total 1) (+ updated 1))
                             ) ;begin
                             (loop (+ i 1) (+ total 1) updated)
                           ) ;if
                         ) ;begin
                         (loop (+ i 1) total updated)
                       ) ;if
                     ) ;let
                    ) ;
                    ((path-dir? entry)
                     (call-with-values (lambda () (fix-directory (path->string entry)))
                       (lambda (sub-total sub-updated)
                         (loop (+ i 1) (+ total sub-total) (+ updated sub-updated))
                       ) ;lambda
                     ) ;call-with-values
                    ) ;
                    (else (loop (+ i 1) total updated))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (make-fix-arg-parser)
      (let ((parser (make-argument-parser '((command . "fix")
                                            (skip-value-options "-m"
                                              "--mode"
                                              "-I"
                                              "-A")
                                            (skip-prefix-options "-m="
                                              "--mode=")
                                            (unknown-options . positional))
                    ) ;make-argument-parser
            ) ;parser
           ) ;
        (parser :add-argument '((name . "help")
                                (short . "h")
                                (action . store-true)))
        (parser :add-argument '((name . "dry-run") (action . store-true)))
        parser
      ) ;let
    ) ;define

    (define (first-positional parser)
      (let ((positionals (parser :positionals)))
        (if (null? positionals) "" (car positionals))
      ) ;let
    ) ;define

    (define (main)
      (let ((parser (make-fix-arg-parser)))
        (parser :parse-argv (argv))
        (let ((help-flag (parser 'help))
              (dry-run (parser 'dry-run))
              (path-str (first-positional parser))
             ) ;
          (cond ((or help-flag (string=? path-str "")) (display-help) #t)
                ((path-file? (path path-str))
                 (if dry-run
                   (fix-file-dry-run path-str)
                   (let ((changed? (fix-file path-str)))
                     (display (string-append "Processing: " path-str))
                     (newline)
                     (if changed?
                       (begin
                         (display (string-append "  Updated: " path-str))
                         (newline)
                       ) ;begin
                       '()
                     ) ;if
                     (display (string-append "Total files processed: 1, Files updated: "
                                (if changed? "1" "0")
                              ) ;string-append
                     ) ;display
                     (newline)
                     #t
                   ) ;let
                 ) ;if
                ) ;
                ((path-dir? (path path-str))
                 (if dry-run
                   (begin
                     (display "错误: --dry-run 选项仅支持单个文件")
                     (newline)
                     (exit 1)
                   ) ;begin
                   (call-with-values (lambda () (fix-directory path-str))
                     (lambda (total updated)
                       (display (string-append "Total files processed: "
                                  (number->string total)
                                  ", Files updated: "
                                  (number->string updated)
                                ) ;string-append
                       ) ;display
                       (newline)
                       #t
                     ) ;lambda
                   ) ;call-with-values
                 ) ;if
                ) ;
                (else (display (string-append "错误: 路径不存在 - " path-str))
                  (newline)
                  (exit 1)
                ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
