(define-library (liii goldfix)
  (export main
          fix-string
          repair-parentheses
          parentheses-balanced?)
  (import (liii base)
          (liii sys)
          (liii path)
          (srfi srfi-13)
          (liii goldfix-repair)
          (liii goldfix-record))

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
      (display "  gf fix /path/to/dir           递归修正目录下所有 .scm 文件")
      (newline))

    (define (contains? lst item)
      (if (null? lst)
          #f
          (or (string=? (car lst) item)
              (contains? (cdr lst) item))))

    (define (extract-positional args)
      (cond
        ((null? args) "")
        ((or (string=? (car args) "-h")
             (string=? (car args) "--help")
             (string=? (car args) "--dry-run"))
         (extract-positional (cdr args)))
        (else (car args))))

    (define (display-diagnostics diagnostics)
      (let loop ((rest diagnostics))
        (if (not (null? rest))
            (begin
              (display "诊断: ")
              (write (car rest))
              (newline)
              (loop (cdr rest))))))

    (define (repair-source source)
      (call-with-values
        (lambda () (repair-parentheses source))
        (lambda (repaired report)
          (if (repair-report-ok? report)
              repaired
              (begin
                (display-diagnostics (repair-report-diagnostics report))
                repaired)))))

    (define (fix-file-dry-run path-str)
      (let ((source (path-read-text (path path-str))))
        (display (repair-source source))
        (newline)))

    (define (fix-file path-str)
      (let* ((p (path path-str))
             (source (path-read-text p))
             (repaired (repair-source source)))
        (if (string=? source repaired)
            #f  ; 未修改
            (begin
              (path-write-text p repaired)
              #t))))  ; 已修改

    (define (fix-directory dir-path)
      (let ((entries (path-list-path (path dir-path))))
        (let loop ((i 0)
                   (total 0)
                   (updated 0))
          (if (>= i (vector-length entries))
              (values total updated)
              (let ((entry (vector-ref entries i)))
                (cond
                  ((path-file? entry)
                   (let ((entry-str (path->string entry)))
                     (if (string-suffix? ".scm" entry-str)
                         (begin
                           (display (string-append "Processing: " entry-str))
                           (newline)
                           (if (fix-file entry-str)
                               (begin
                                 (display (string-append "  Updated: " entry-str))
                                 (newline)
                                 (loop (+ i 1) (+ total 1) (+ updated 1)))
                               (loop (+ i 1) (+ total 1) updated)))
                         (loop (+ i 1) total updated))))
                  ((path-dir? entry)
                   (call-with-values
                     (lambda () (fix-directory (path->string entry)))
                     (lambda (sub-total sub-updated)
                       (loop (+ i 1) (+ total sub-total) (+ updated sub-updated)))))
                  (else
                    (loop (+ i 1) total updated))))))))

    (define (main)
      (let* ((args (cddr (argv)))
             (help-flag (or (contains? args "-h") (contains? args "--help")))
             (dry-run (contains? args "--dry-run"))
             (path-str (extract-positional args)))
        (cond
          ((or help-flag (string=? path-str ""))
           (display-help)
           #t)
           ((path-file? (path path-str))
            (if dry-run
                (fix-file-dry-run path-str)
                (let ((changed? (fix-file path-str)))
                  (display (string-append "Processing: " path-str))
                  (newline)
                  (if changed?
                      (begin
                        (display (string-append "  Updated: " path-str))
                        (newline))
                      '())
                  (display (string-append "Total files processed: 1, Files updated: "
                                          (if changed? "1" "0")))
                  (newline)
                  #t)))
           ((path-dir? (path path-str))
            (if dry-run
                (begin
                  (display "错误: --dry-run 选项仅支持单个文件")
                  (newline)
                  (exit 1))
                (call-with-values
                  (lambda () (fix-directory path-str))
                  (lambda (total updated)
                    (display (string-append "Total files processed: "
                                            (number->string total)
                                            ", Files updated: "
                                            (number->string updated)))
                    (newline)
                    #t))))
          (else
           (display (string-append "错误: 路径不存在 - " path-str))
           (newline)
           (exit 1)))))
  ) ;begin
) ;define-library
