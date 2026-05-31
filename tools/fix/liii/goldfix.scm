(define-library (liii goldfix)
  (export main fix-string repair-parentheses parentheses-balanced?)
  (import (liii base)
    (liii sys)
    (liii path)
    (liii string)
    (liii argparse)
    (liii hashlib)
    (liii os)
    (srfi srfi-13)
    (liii goldfix-repair)
    (liii goldfix-record)
  ) ;import

  (begin

    (define (fix-cache-base-dir)
      (path->string (path-join (path-home) ".cache" "goldfish" "fix" (version)))
    ) ;define

    (define (fix-cache-path file-path)
      (let* ((hash (sha256-by-file file-path))
             (prefix (substring hash 0 2))
             (rest (substring hash 2))
             (base (fix-cache-base-dir))
            ) ;
        (path->string (path-join base prefix rest))
      ) ;let*
    ) ;define

    (define (fix-cache-hit? file-path)
      (let ((cache (fix-cache-path file-path)))
        (file-exists? cache)
      ) ;let
    ) ;define

    (define (fix-cache-touch file-path)
      (let* ((cache (fix-cache-path file-path))
             (cache-dir (path->string (path-parent (path cache))))
            ) ;
        (unless (path-dir? (path cache-dir))
          (g_mkdir cache-dir)
        ) ;unless
        (path-touch (path cache))
      ) ;let*
    ) ;define

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
      (display "  -e, --extension EXT    指定文件后缀名（默认 scm，支持逗号分隔多个）")
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
      (display "  gf fix -e sld /path/to/dir    递归修正目录下所有 .sld 文件"
      ) ;display
      (newline)
      (display "  gf fix -e scm,sld /path/to/dir  递归修正目录下所有 .scm 和 .sld 文件"
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

    (define* (fix-file-core path-str (use-cache? #t))
      (if (and use-cache? (fix-cache-hit? path-str))
        'cached
        (let* ((p (path path-str))
               (source (path-read-text p))
               (repaired (repair-source source))
              ) ;
          (if (string=? source repaired)
            (begin
              (when use-cache?
                (fix-cache-touch path-str)
              ) ;when
              #f
            ) ;begin
            (begin
              (path-write-text p repaired)
              (when use-cache?
                (fix-cache-touch path-str)
              ) ;when
              #t
            ) ;begin
          ) ;if
        ) ;let*
      ) ;if
    ) ;define*

    (define* (fix-file path-str (use-cache? #t))
      (let ((fmt-cmd (string-append (executable) " fmt " path-str)))
        (os-call fmt-cmd)
      ) ;let
      (fix-file-core path-str use-cache?)
    ) ;define*

    (define (file-extension-match? filename extensions)
      (let loop ((exts extensions))
        (if (null? exts)
          #f
          (if (string-suffix? (car exts) filename)
            #t
            (loop (cdr exts))
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (fix-directory dir-path extensions)
      (let ((fmt-cmd (string-append (executable) " fmt " dir-path)))
        (os-call fmt-cmd)
      ) ;let
      (let ((entries (path-list-path (path dir-path))))
        (let loop
          ((i 0) (total 0) (updated 0) (cached 0))
          (if (>= i (vector-length entries))
            (values total updated cached)
            (let ((entry (vector-ref entries i)))
              (cond ((path-file? entry)
                     (let ((entry-str (path->string entry)))
                       (if (file-extension-match? entry-str extensions)
                         (let ((result (fix-file-core entry-str)))
                           (cond ((eq? result 'cached)
                                  (loop (+ i 1) (+ total 1) updated (+ cached 1)))
                                 (result (display (string-append "  Updated: " entry-str))
                                   (newline)
                                   (loop (+ i 1) (+ total 1) (+ updated 1) cached))
                                 (else (display (string-append "Fixing: " entry-str))
                                   (newline)
                                   (loop (+ i 1) (+ total 1) updated cached))
                           ) ;cond
                         ) ;let
                         (loop (+ i 1) total updated cached)
                       ) ;if
                     ) ;let
                    ) ;
                    ((path-dir? entry)
                     (call-with-values (lambda () (fix-directory (path->string entry) extensions))
                       (lambda (sub-total sub-updated sub-cached)
                         (loop (+ i 1) (+ total sub-total) (+ updated sub-updated) (+ cached sub-cached))
                       ) ;lambda
                     ) ;call-with-values
                    ) ;
                    (else (loop (+ i 1) total updated cached))
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
        (parser :add-argument '((name . "extension")
                                (short . "e")
                                (type . string)
                                (default . "scm")))
        parser
      ) ;let
    ) ;define

    (define (first-positional parser)
      (let ((positionals (parser :positionals)))
        (if (null? positionals) "" (car positionals))
      ) ;let
    ) ;define

    (define (normalize-extension ext)
      (if (and (> (string-length ext) 0)
               (char=? (string-ref ext 0) #\.))
        ext
        (string-append "." ext))
    ) ;define

    (define (parse-extensions raw)
      (map normalize-extension (string-split raw ","))
    ) ;define

    (define (main)
      (let ((parser (make-fix-arg-parser)))
        (parser :parse-argv (argv))
        (let* ((help-flag (parser 'help))
               (dry-run (parser 'dry-run))
               (extensions (parse-extensions (parser 'extension)))
               (path-str (first-positional parser))
              ) ;
          (cond ((or help-flag (string=? path-str "")) (display-help) #t)
                ((path-file? (path path-str))
                 (if dry-run
                   (fix-file-dry-run path-str)
                   (let ((result (fix-file path-str)))
                     (cond ((eq? result 'cached) #f)
                           (result (display (string-append "  Updated: " path-str)) (newline))
                           (else (display (string-append "Fixing: " path-str)) (newline))
                     ) ;cond
                     (display (string-append "Total files fixed: 1, Files updated: "
                                (if (eq? result #t) "1" "0")
                                ", Files cached: "
                                (if (eq? result 'cached) "1" "0")
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
                   (call-with-values (lambda () (fix-directory path-str extensions))
                     (lambda (total updated cached)
                       (display (string-append "Total files fixed: "
                                  (number->string total)
                                  ", Files updated: "
                                  (number->string updated)
                                  ", Files cached: "
                                  (number->string cached)
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
