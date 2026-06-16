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
  (export main fix-string repair-parentheses parentheses-balanced?)
  (import (liii base)
    (liii sys)
    (liii path)
    (liii string)
    (liii json)
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
      (display "      --exclude PATTERN    跳过匹配的文件（路径后缀匹配，逗号分隔多个）"
      ) ;display
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
      (display "  gf fix --exclude=prefix-kbd.scm /path/to/dir  递归修正但跳过 prefix-kbd.scm"
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

    ;; fix-file 内部会先调 gf fmt，因此 --exclude 必须透传，否则被排除文件
    ;; 仍会被 fmt 格式化。
    (define* (fix-file path-str (use-cache? #t) (excludes '()))
      (let ((exclude-arg (if (null? excludes)
                           ""
                           (string-append " --exclude=" (string-join excludes ","))
                         ) ;if
            ) ;exclude-arg
           ) ;
        (os-call (string-append (executable) " fmt" exclude-arg " " path-str))
      ) ;let
      (fix-file-core path-str use-cache?)
    ) ;define*

    (define (fix-file-list files dry-run excludes)
      (let loop
        ((remaining files) (total 0) (updated 0) (cached 0))
        (if (null? remaining)
          (values total updated cached)
          (let ((file (car remaining)))
            (if (file-excluded? file excludes)
              (loop (cdr remaining) total updated cached)
              (if dry-run
                (begin
                  (display (string-append "Fixing: " file))
                  (newline)
                  (fix-file-dry-run file)
                  (loop (cdr remaining) (+ total 1) updated cached)
                ) ;begin
                (let ((result (fix-file file #t excludes)))
                  (cond ((eq? result 'cached) (loop (cdr remaining) (+ total 1) updated (+ cached 1)))
                        (result (display (string-append "  Updated: " file))
                          (newline)
                          (loop (cdr remaining) (+ total 1) (+ updated 1) cached)
                        ) ;result
                        (else (display (string-append "Fixing: " file))
                          (newline)
                          (loop (cdr remaining) (+ total 1) updated cached)
                        ) ;else
                  ) ;cond
                ) ;let
              ) ;if
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

    ;; 把 Windows 反斜杠归一化为正斜杠，跨平台一致比较。
    (define (normalize-sep path-str)
      (list->string (let loop
                      ((i (- (string-length path-str) 1)) (acc '()))
                      (if (< i 0)
                        acc
                        (let ((ch (string-ref path-str i)))
                          (loop (- i 1) (cons (if (char=? ch #\\) #\/ ch) acc))
                        ) ;let
                      ) ;if
                    ) ;let
      ) ;list->string
    ) ;define

    ;; entry 是否命中 excludes：归一化后按"分隔符边界 + 后缀"匹配。
    ;; pattern 含 / 时按完整相对路径精确匹配（不会误伤同名文件）；
    ;; pattern 不含 / 时退化为 basename 匹配（向后兼容）。
    (define (file-excluded? entry-str excludes)
      (let ((entry-norm (normalize-sep entry-str)))
        (let loop
          ((pats excludes))
          (if (null? pats)
            #f
            (let* ((p (normalize-sep (car pats)))
                   (plen (string-length p))
                   (elen (string-length entry-norm))
                  ) ;
              (if (or (string=? entry-norm p)
                    (and (>= elen (+ plen 1))
                      (char=? (string-ref entry-norm (- elen plen 1)) #\/)
                      (string-suffix? p entry-norm)
                    ) ;and
                  ) ;or
                #t
                (loop (cdr pats))
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 解析 --exclude 的逗号分隔值，丢弃空串。
    (define (parse-excludes raw)
      (if (or (not raw) (string=? raw ""))
        '()
        (let loop
          ((parts (string-split raw ",")) (acc '()))
          (if (null? parts)
            (reverse acc)
            (let ((p (car parts)))
              (if (string=? p "") (loop (cdr parts) acc) (loop (cdr parts) (cons p acc)))
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; 自动检测项目根（gfproject.json 所在目录）下的 gfexclude.json。
    ;; 文件格式：JSON 对象，形如
    ;;   {"exclude": [
    ;;     {"path": "a/b.scm", "reason": "..."},
    ;;     "c/d.scm"
    ;;   ]}
    ;; exclude 数组每项可以是字符串（纯路径）或对象（含 path 与可选 reason）。
    ;; 存在且可解析则返回 pattern 列表；不存在或无项目根时返回 '()。
    ;; Why: 让普通的 gf fmt / gf fix 也尊重项目级 exclude 配置，无需传参；
    ;;      每条可带 reason 说明为何排除，便于维护。
    ;; 畸形 JSON / 类型不符时打 warning 并返回 '()，避免整个 gf fix 崩溃。
    (define (exclude-entry->path entry)
      (if (string? entry) entry (json-ref entry "path"))
    ) ;define

    ;; 纯解析：从已读入的 JSON 文本构造 exclude pattern 列表。
    ;; 出错时抛异常（由调用方 catch）。
    (define (parse-exclude-json text)
      (let* ((obj (string->json text)) (arr (json-ref obj "exclude")))
        (if (not (vector? arr))
          '()
          (let loop
            ((i 0) (acc '()))
            (if (>= i (vector-length arr))
              (reverse acc)
              (let ((p (exclude-entry->path (vector-ref arr i))))
                (loop (+ i 1) (if (string? p) (cons p acc) acc))
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (read-project-excludes)
      (let ((root (g_project-root)))
        (if (or (not root) (not (string? root)) (string=? root ""))
          '()
          (let ((file (string-append root "/gfexclude.json")))
            (if (not (file-exists? file))
              '()
              (catch #t
                (lambda () (parse-exclude-json (path-read-text (path file))))
                (lambda (type info)
                  (let ((e (if (null? info) type (car info))))
                    (display (string-append "warning: gfexclude.json 解析失败，已忽略 - "
                               (if (string? e) e (object->string e))
                             ) ;string-append
                    ) ;display
                    (newline)
                    '()
                  ) ;let
                ) ;lambda
              ) ;catch
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (fix-changed-since since path-str dry-run extensions excludes)
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
                        (call-with-values (lambda () (fix-file-list files dry-run excludes))
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

    (define (fix-directory dir-path extensions excludes)
      ;; 先对整目录跑一次 gf fmt（与原逻辑一致），--exclude 同样透传，
      ;; 防止被排除文件在 fmt 阶段被改动。
      (let ((exclude-arg (if (null? excludes)
                           ""
                           (string-append " --exclude=" (string-join excludes ","))
                         ) ;if
            ) ;exclude-arg
           ) ;
        (os-call (string-append (executable) " fmt" exclude-arg " " dir-path))
      ) ;let
      (let ((entries (path-list-path (path dir-path))))
        (let loop
          ((i 0) (total 0) (updated 0) (cached 0))
          (if (>= i (vector-length entries))
            (values total updated cached)
            (let ((entry (vector-ref entries i)))
              (cond ((path-file? entry)
                     (let ((entry-str (path->string entry)))
                       (if (and (file-extension-match? entry-str extensions)
                             (not (file-excluded? entry-str excludes))
                           ) ;and
                         (let ((result (fix-file-core entry-str)))
                           (cond ((eq? result 'cached) (loop (+ i 1) (+ total 1) updated (+ cached 1)))
                                 (result (display (string-append "  Updated: " entry-str))
                                   (newline)
                                   (loop (+ i 1) (+ total 1) (+ updated 1) cached)
                                 ) ;result
                                 (else (display (string-append "Fixing: " entry-str))
                                   (newline)
                                   (loop (+ i 1) (+ total 1) updated cached)
                                 ) ;else
                           ) ;cond
                         ) ;let
                         (loop (+ i 1) total updated cached)
                       ) ;if
                     ) ;let
                    ) ;
                    ((path-dir? entry)
                     (call-with-values (lambda () (fix-directory (path->string entry) extensions excludes))
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
        (parser :add-argument '((name . "exclude") (type . string)))
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
               (excludes (append (parse-excludes (parser 'exclude)) (read-project-excludes)))
               (path-str (first-positional parser))
              ) ;
          (cond (help-flag (display-help) #t)
                (changed-since (fix-changed-since changed-since path-str dry-run extensions excludes)
                ) ;changed-since
                ((string=? path-str "") (display-help) #t)
                ((path-file? (path path-str))
                 (if (file-excluded? path-str excludes)
                   (begin
                     (display (string-append "Skipped (excluded): " path-str))
                     (newline)
                     #t
                   ) ;begin
                   (if dry-run
                     (fix-file-dry-run path-str)
                     (let ((result (fix-file path-str #t excludes)))
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
                 ) ;if
                ) ;
                ((path-dir? (path path-str))
                 (if dry-run
                   (begin
                     (display "错误: --dry-run 选项仅支持单个文件")
                     (newline)
                     (exit 1)
                   ) ;begin
                   (call-with-values (lambda () (fix-directory path-str extensions excludes))
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
