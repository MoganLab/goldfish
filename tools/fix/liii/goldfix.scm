(define-library (liii goldfix)
  (export main
    fix-string
    repair-parentheses
    parentheses-balanced?
  ) ;export
  (import (liii base)
    (liii sys)
    (liii path)
    (srfi srfi-13)
    (liii goldfix-repair)
    (liii goldfix-record)
  ) ;import

  (begin
    (define (display-help)
      (display "Usage: gf fix [options] [path]"
      ) ;display
      (newline)
      (newline)
      (display "Options:")
      (newline)
      (display "  -h, --help       显示此帮助文档"
      ) ;display
      (newline)
      (display "      --dry-run    预览模式（仅支持单个文件）"
      ) ;display
      (newline)
      (newline)
      (display "Arguments:")
      (newline)
      (display "  path    要修正括号的文件或目录路径（可选）"
      ) ;display
      (newline)
      (newline)
      (display "Examples:")
      (newline)
      (display "  gf fix                        显示此帮助文档"
      ) ;display
      (newline)
      (display "  gf fix --help                 显示此帮助文档"
      ) ;display
      (newline)
      (display "  gf fix file.scm               修正单个文件"
      ) ;display
      (newline)
      (display "  gf fix --dry-run file.scm     预览修正结果"
      ) ;display
      (newline)
      (display "  gf fix /path/to/dir           递归修正目录下所有 .scm 文件"
      ) ;display
      (newline)
    ) ;define

    (define (contains? lst item)
      (if (null? lst)
        #f
        (or (string=? (car lst) item)
          (contains? (cdr lst) item)
        ) ;or
      ) ;if
    ) ;define

    (define (extract-positional args)
      (cond ((null? args) "")
            ((or (string=? (car args) "-h")
               (string=? (car args) "--help")
               (string=? (car args) "--dry-run")
             ) ;or
             (extract-positional (cdr args))
            ) ;
            (else (car args))
      ) ;cond
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
              (display-diagnostics (repair-report-diagnostics report)
              ) ;display-diagnostics
              repaired
            ) ;begin
          ) ;if
        ) ;lambda
      ) ;call-with-values
    ) ;define

    (define (fix-file-dry-run path-str)
      (let ((source (path-read-text (path path-str))
            ) ;source
           ) ;
        (display (repair-source source))
        (newline)
      ) ;let
    ) ;define

    (define (fix-file path-str)
      (let* ((p (path path-str))
             (source (path-read-text p))
             (repaired (repair-source source))
            ) ;
        (if (string=? source repaired)
          #f
          (begin
            (path-write-text p repaired)
            #t
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    (define (fix-directory dir-path)
      (let ((entries (path-list-path (path dir-path))
            ) ;entries
           ) ;
        (let loop
          ((i 0) (total 0) (updated 0))
          (if (>= i (vector-length entries))
            (values total updated)
            (let ((entry (vector-ref entries i)))
              (cond ((path-file? entry)
                     (let ((entry-str (path->string entry)))
                       (if (string-suffix? ".scm" entry-str)
                         (begin
                           (display (string-append "Processing: " entry-str)
                           ) ;display
                           (newline)
                           (if (fix-file entry-str)
                             (begin
                               (display (string-append "  Updated: " entry-str)
                               ) ;display
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
                     (call-with-values (lambda ()
                                         (fix-directory (path->string entry))
                                       ) ;lambda
                       (lambda (sub-total sub-updated)
                         (loop (+ i 1)
                           (+ total sub-total)
                           (+ updated sub-updated)
                         ) ;loop
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

    (define (main)
      (let* ((args (cddr (argv)))
             (help-flag (or (contains? args "-h")
                          (contains? args "--help")
                        ) ;or
             ) ;help-flag
             (dry-run (contains? args "--dry-run"))
             (path-str (extract-positional args))
            ) ;
        (cond ((or help-flag (string=? path-str ""))
               (display-help)
               #t
              ) ;
              ((path-file? (path path-str))
               (if dry-run
                 (fix-file-dry-run path-str)
                 (let ((changed? (fix-file path-str)))
                   (display (string-append "Processing: " path-str)
                   ) ;display
                   (newline)
                   (if changed?
                     (begin
                       (display (string-append "  Updated: " path-str)
                       ) ;display
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
                   (display "错误: --dry-run 选项仅支持单个文件"
                   ) ;display
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
              (else (display (string-append "错误: 路径不存在 - "
                               path-str
                             ) ;string-append
                    ) ;display
                (newline)
                (exit 1)
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library
