(define (%goldfix-common-dirname path-str)
  (let loop
    ((i (- (string-length path-str) 1)))
    (cond ((< i 0) ".")
          ((or (char=? (string-ref path-str i) #\/) (char=? (string-ref path-str i) #\\))
           (if (= i 0) "." (substring path-str 0 i))
          ) ;
          (else (loop (- i 1)))
    ) ;cond
  ) ;let
) ;define

(set! *load-path*
  (append (list "../common" "tools/common")
    (map (lambda (root) (string-append (%goldfix-common-dirname root) "/common"))
      *load-path*
    ) ;map
    *load-path*
  ) ;append
) ;set!

(define-library (liii goldfix)
  (export main
    fix-string
    repair-source
    repair-source*
    repair-parentheses
    parentheses-balanced?
  ) ;export
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
    (liii goldtool-changed)
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
      (display "  -e, --extension EXT    指定文件后缀名（默认 scm，支持逗号分隔多个）"
      ) ;display
      (newline)
      (display "      --changed-since REV    仅修正自 REV 以来变更的文件")
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
      (display "  gf fix --changed-since=HEAD   修正自 HEAD 以来变更的文件")
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
      (newline)
      (display "注意: fix 依赖规整的缩进来推断括号结构，建议先运行 gf fmt 再 fix。"
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

    ;; fix 是缩进驱动的：只有当缩进可信（已 gf fmt）时，按缩进推断括号才安全。
    ;; 最危险的不可信信号是"括号本已平衡，fix 却想改动"——此时任何调整都只会
    ;; 破坏正确的结构。检测到此情形时拒绝修改、原样返回，并提示用户先 fmt，
    ;; 而不是静默破坏代码。
    ;; 返回两个值：(repaired skipped?)——skipped? 为 #t 表示因缩进不可信被守卫拦截，
    ;; 调用方可据此向用户区分"本就平衡无需改"与"被守卫拒绝"。
    (define (repair-source* source)
      (call-with-values (lambda () (repair-parentheses source))
        (lambda (repaired report)
          (let ((edits (repair-report-edits report)))
            (if (and (parentheses-balanced? source) (not (null? edits)))
              (begin
                (display "警告: 括号已平衡但检测到缩进可能不规范，fix 可能破坏代码结构。"
                ) ;display
                (newline)
                (display "请先运行 gf fmt 规整缩进后再使用 gf fix。")
                (newline)
                (values source #t)
              ) ;begin
              (if (repair-report-ok? report)
                (values repaired #f)
                (begin
                  (display-diagnostics (repair-report-diagnostics report))
                  (values repaired #f)
                ) ;begin
              ) ;if
            ) ;if
          ) ;let
        ) ;lambda
      ) ;call-with-values
    ) ;define

    ;; 对外保持单返回值契约（仅返回修正后的 source）。
    (define (repair-source source)
      (call-with-values (lambda () (repair-source* source))
        (lambda (repaired skipped?) repaired)
      ) ;call-with-values
    ) ;define

    (define (fix-file-dry-run path-str)
      (let ((source (path-read-text (path path-str))))
        (display (repair-source source))
        (newline)
      ) ;let
    ) ;define

    ;; 返回值区分四种结果：
    ;;   'cached   命中缓存，跳过
    ;;   'updated  实际改动了文件（写盘）
    ;;   'balanced 未改动——括号本就平衡、缩进正常，无需修正
    ;;   'skipped  未改动——缩进不可信，被守卫拦截（已打印"请先 gf fmt"）
    (define* (fix-file-core path-str (use-cache? #t))
      (if (and use-cache? (fix-cache-hit? path-str))
        'cached
        (let* ((p (path path-str)) (source (path-read-text p)))
          (call-with-values (lambda () (repair-source* source))
            (lambda (repaired skipped?)
              (if (string=? source repaired)
                (begin
                  (when use-cache?
                    (fix-cache-touch path-str)
                  ) ;when
                  (if skipped? 'skipped 'balanced)
                ) ;begin
                (begin
                  (path-write-text p repaired)
                  (when use-cache?
                    (fix-cache-touch path-str)
                  ) ;when
                  'updated
                ) ;begin
              ) ;if
            ) ;lambda
          ) ;call-with-values
        ) ;let*
      ) ;if
    ) ;define*

    ;; 职责单一：fix 只做括号修正，不再隐式调用 gf fmt。
    (define* (fix-file path-str (use-cache? #t))
      (fix-file-core path-str use-cache?)
    ) ;define*

    (define (fix-file-list files dry-run)
      (let loop
        ((remaining files) (total 0) (updated 0) (cached 0))
        (if (null? remaining)
          (values total updated cached)
          (let ((file (car remaining)))
            (if dry-run
              (begin
                (display (string-append "Previewing: " file))
                (newline)
                (fix-file-dry-run file)
                (loop (cdr remaining) (+ total 1) updated cached)
              ) ;begin
              (let ((result (fix-file file #t)))
                (cond ((eq? result 'cached) (loop (cdr remaining) (+ total 1) updated (+ cached 1)))
                      ((eq? result 'updated)
                       (display (string-append "  Updated: " file))
                       (newline)
                       (loop (cdr remaining) (+ total 1) (+ updated 1) cached)
                      ) ;
                      ((eq? result 'skipped)
                       (display (string-append "  Skipped (run gf fmt first): " file))
                       (newline)
                       (loop (cdr remaining) (+ total 1) updated cached)
                      ) ;
                      (else (display (string-append "  Already balanced: " file))
                        (newline)
                        (loop (cdr remaining) (+ total 1) updated cached)
                      ) ;else
                ) ;cond
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (file-extension-match? filename extensions)
      (let loop
        ((exts extensions))
        (if (null? exts)
          #f
          (if (string-suffix? (car exts) filename) #t (loop (cdr exts)))
        ) ;if
      ) ;let
    ) ;define

    (define (fix-changed-since since path-str dry-run extensions)
      (let ((scope (if (string=? path-str "") #f path-str)))
        (cond ((and scope (not (or (path-file? (path scope)) (path-dir? (path scope)))))
               (display (string-append "错误: 路径不存在 - " scope))
               (newline)
               (exit 1)
              ) ;
              (else (let ((files (if scope
                                   (changed-scheme-files-since since scope extensions)
                                   (changed-scheme-files-since since #f extensions)
                                 ) ;if
                          ) ;files
                         ) ;
                      (if (null? files)
                        (begin
                          (display (string-append "No changed Scheme files since " since))
                          (newline)
                          #t
                        ) ;begin
                        (call-with-values (lambda () (fix-file-list files dry-run))
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
                    ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (fix-directory dir-path extensions)
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
                           (cond ((eq? result 'cached) (loop (+ i 1) (+ total 1) updated (+ cached 1)))
                                 ((eq? result 'updated)
                                  (display (string-append "  Updated: " entry-str))
                                  (newline)
                                  (loop (+ i 1) (+ total 1) (+ updated 1) cached)
                                 ) ;
                                 ((eq? result 'skipped)
                                  (display (string-append "  Skipped (run gf fmt first): " entry-str))
                                  (newline)
                                  (loop (+ i 1) (+ total 1) updated cached)
                                 ) ;
                                 (else (loop (+ i 1) (+ total 1) updated cached))
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
        (parser :add-argument
          '((name . "extension")
            (short . "e")
            (type . string)
            (default . "scm"))
        ) ;parser
        (parser :add-argument '((name . "changed-since") (type . string)))
        parser
      ) ;let
    ) ;define

    (define (first-positional parser)
      (let ((positionals (parser :positionals)))
        (if (null? positionals) "" (car positionals))
      ) ;let
    ) ;define

    (define (normalize-extension ext)
      (if (and (> (string-length ext) 0) (char=? (string-ref ext 0) #\.))
        ext
        (string-append "." ext)
      ) ;if
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
               (changed-since (parser 'changed-since))
               (path-str (first-positional parser))
              ) ;
          (cond (help-flag (display-help) #t)
                (changed-since (fix-changed-since changed-since path-str dry-run extensions))
                ((string=? path-str "") (display-help) #t)
                ((path-file? (path path-str))
                 (if dry-run
                   (fix-file-dry-run path-str)
                   (let ((result (fix-file path-str #t)))
                     (cond ((eq? result 'cached) #f)
                           ((eq? result 'updated)
                            (display (string-append "  Updated: " path-str))
                            (newline)
                           ) ;
                           ((eq? result 'skipped)
                            (display (string-append "  Skipped (run gf fmt first): " path-str))
                            (newline)
                           ) ;
                           (else (display (string-append "  Already balanced: " path-str)) (newline))
                     ) ;cond
                     (display (string-append "Total files fixed: 1, Files updated: "
                                (if (eq? result 'updated) "1" "0")
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
        ) ;let*
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
