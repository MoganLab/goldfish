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
        (path-write-text p repaired)))

    (define (fix-directory dir-path)
      (let ((entries (path-list-path (path dir-path))))
        (vector-for-each
          (lambda (entry)
            (cond
              ((path-file? entry)
               (let ((entry-str (path->string entry)))
                 (if (string-suffix? ".scm" entry-str)
                     (begin
                       (fix-file entry-str)
                       (display (string-append "已修正: " entry-str))
                       (newline)))))
              ((path-dir? entry)
               (fix-directory (path->string entry)))))
          entries)))

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
               (begin
                 (fix-file path-str)
                 (display (string-append "已修正: " path-str))
                 (newline)
                 #t)))
          ((path-dir? (path path-str))
           (if dry-run
               (begin
                 (display "错误: --dry-run 选项仅支持单个文件")
                 (newline)
                 (exit 1))
               (begin
                 (fix-directory path-str)
                 #t)))
          (else
           (display (string-append "错误: 路径不存在 - " path-str))
           (newline)
           (exit 1)))))
  ) ;begin
) ;define-library
