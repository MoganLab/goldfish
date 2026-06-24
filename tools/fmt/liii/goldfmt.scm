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

;; gf fmt：多语言代码格式化工具。
;; - 无路径参数：读 gf_fmt.json 仓库批量格式化所有语言（原 gf format 的能力）。
;; - 无路径参数 + --check：非破坏性检查，未格式化则退出码 1（原 gf format --check）。
;; - 有路径参数：按后缀（或 -e）选语言，格式化单文件或递归目录。
;; 各语言实现见 (liii scheme-fmt) / (liii cpp-fmt)，配置见 (liii goldfmt-config)。

(define (%goldfmt-common-dirname path-str)
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
    (map (lambda (root) (string-append (%goldfmt-common-dirname root) "/common"))
      *load-path*
    ) ;map
    *load-path*
  ) ;append
) ;set!

(define-library (liii goldfmt)
  (import (liii base)
    (liii sys)
    (liii os)
    (liii path)
    (liii string)
    (liii argparse)
    (liii goldfmt-scan)
    (liii goldfmt-format)
    (liii goldfmt-lang)
    (liii goldfmt-config)
    (liii scheme-fmt)
    (liii cpp-fmt)
  ) ;import
  (export main format-datum format-datum+node format-node format-string)
  (begin

    ;; ---- 参数解析 -------------------------------------------------------
    (define (normalize-extension ext)
      (if (and (> (string-length ext) 0) (char=? (string-ref ext 0) #\.))
        ext
        (string-append "." ext)
      ) ;if
    ) ;define

    (define (parse-extensions raw)
      (map normalize-extension (string-split raw ","))
    ) ;define

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

    (define (make-fmt-arg-parser)
      (let ((parser (make-argument-parser '((command . "fmt")
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
        (parser :add-argument '((name . "check") (action . store-true)))
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

    ;; ---- 帮助 -----------------------------------------------------------
    (define (display-help)
      (display "Usage: gf fmt [options] [path]")
      (newline)
      (newline)
      (display "Options:")
      (newline)
      (display "  -h, --help       显示此帮助文档")
      (newline)
      (display "      --check      非破坏性检查：发现未格式化文件则退出码 1（供 CI，读 gf_fmt.json）"
      ) ;display
      (newline)
      (display "      --dry-run    预览模式（不写回文件；目录路径不支持）"
      ) ;display
      (newline)
      (display "  -e, --extension EXT    指定文件后缀名（默认 scm，支持逗号分隔多个）"
      ) ;display
      (newline)
      (display "      --changed-since REV    仅格式化自 REV 以来变更的 Scheme 文件"
      ) ;display
      (newline)
      (display "      --exclude PATTERN    跳过匹配的文件（路径后缀匹配，逗号分隔多个）"
      ) ;display
      (newline)
      (newline)
      (display "Arguments:")
      (newline)
      (display "  path    要格式化的文件或目录路径（可选）")
      (newline)
      (display "          无 path 时：读 gf_fmt.json 仓库批量格式化所有语言（scheme + cpp）"
      ) ;display
      (newline)
      (newline)
      (display "Examples:")
      (newline)
      (display "  gf fmt                       仓库批量格式化（C++ + Scheme）"
      ) ;display
      (newline)
      (display "  gf fmt --check               CI 非破坏性检查，未格式化退出码 1"
      ) ;display
      (newline)
      (display "  gf fmt file.scm              格式化单个文件")
      (newline)
      (display "  gf fmt --dry-run file.scm    预览格式化结果")
      (newline)
      (display "  gf fmt /path/to/dir          递归格式化目录下所有 .scm 文件"
      ) ;display
      (newline)
      (display "  gf fmt -e scm,sld dir/       递归格式化目录下所有 .scm 和 .sld 文件"
      ) ;display
      (newline)
      (display "  gf fmt --changed-since=HEAD  格式化自 HEAD 以来变更的 Scheme 文件"
      ) ;display
      (newline)
    ) ;define

    ;; ---- 仓库批量格式化（无路径参数）-----------------------------------
    ;; 遍历 gf_fmt.json 各语言，逐语言收集 + 格式化。
    (define (flush-output)
      (flush-output-port (current-output-port))
    ) ;define

    (define (run-repo-format cfg)
      (let loop
        ((langs (lang-names)))
        (if (null? langs)
          (begin
            (display "Done.")
            (newline)
            #t
          ) ;begin
          (let* ((lang (car langs))
                 (parts (config-for-lang lang cfg))
                 (suffixes (car parts))
                 (paths (cadr parts))
                 (excludes (caddr parts))
                ) ;
            (cond ((eq? lang 'scheme)
                   (display "=== Formatting Scheme files ===")
                   (newline)
                   (flush-output)
                   (let ((files (collect-scheme paths excludes)))
                     (call-with-values (lambda () (format-file-list files #f excludes))
                       (lambda (total updated cached)
                         (display (string-append "Total files formatted: "
                                    (number->string total)
                                    ", Files updated: "
                                    (number->string updated)
                                    ", Files cached: "
                                    (number->string cached)
                                  ) ;string-append
                         ) ;display
                         (newline)
                       ) ;lambda
                     ) ;call-with-values
                   ) ;let
                  ) ;
                  ((eq? lang 'cpp)
                   (display "=== Formatting C++ files ===")
                   (newline)
                   (flush-output)
                   (let ((files (collect-cpp paths suffixes excludes)))
                     (format-cpp-files files)
                   ) ;let
                  ) ;
            ) ;cond
            (newline)
            (loop (cdr langs))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; ---- 仓库级非破坏检查（无路径参数 + --check）-----------------------
    ;; 迁移自原 gf format --check：逐语言收集 + 逐文件检查，汇总 offenders。
    ;; Windows 无 sh 时 cpp 检查跳过并整体退出 0（CI 在 Debian 跑）。
    (define (shell-quote s)
      (string-append "'" (string-replace s "'" "'\\''") "'")
    ) ;define

    (define (print-offenders label offenders)
      (let ((n (length offenders)))
        (if (= n 0)
          (begin
            (display (string-append label ": OK"))
            (newline)
          ) ;begin
          (begin
            (display (string-append label ": " (number->string n) " file(s) need formatting")
            ) ;display
            (newline)
            (let loop
              ((fs offenders))
              (if (null? fs)
                #t
                (begin
                  (display (string-append "  " (car fs)))
                  (newline)
                  (loop (cdr fs))
                ) ;begin
              ) ;if
            ) ;let
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    ;; 逐文件检查某语言，返回 offenders 列表。
    ;; scheme：gf fmt --dry-run 与磁盘逐字节比；cpp：clang-format --dry-run --Werror 退出码。
    (define (check-lang lang cfg)
      (let* ((parts (config-for-lang lang cfg))
             (suffixes (car parts))
             (paths (cadr parts))
             (excludes (caddr parts))
            ) ;
        (cond ((eq? lang 'scheme)
               (let ((files (collect-scheme paths excludes)))
                 (let loop
                   ((fs files) (bad '()))
                   (if (null? fs)
                     (reverse bad)
                     (let ((f (car fs)))
                       (loop (cdr fs) (if (check-scheme-file f excludes) bad (cons f bad)))
                     ) ;let
                   ) ;if
                 ) ;let
               ) ;let
              ) ;
              ((eq? lang 'cpp)
               (if (os-windows?)
                 '()
                 (let ((files (collect-cpp paths suffixes excludes)))
                   (let loop
                     ((fs files) (bad '()))
                     (if (null? fs)
                       (reverse bad)
                       (let ((f (car fs)))
                         (loop (cdr fs) (if (check-cpp-file f) bad (cons f bad)))
                       ) ;let
                     ) ;if
                   ) ;let
                 ) ;let
               ) ;if
              ) ;
              (else '())
        ) ;cond
      ) ;let*
    ) ;define

    (define (run-repo-check cfg)
      (if (os-windows?)
        (begin
          (display "fmt --check: skipped on Windows (requires sh).")
          (newline)
          (exit 0)
        ) ;begin
        (let loop
          ((langs (lang-names)) (results '()))
          (if (null? langs)
            (let* ((scheme-bad (cdr (assq 'scheme results)))
                   (cpp-bad (cdr (assq 'cpp results)))
                   (total (+ (length scheme-bad) (length cpp-bad)))
                  ) ;
              (newline)
              (print-offenders "Scheme" scheme-bad)
              (print-offenders "C++" cpp-bad)
              (newline)
              (if (> total 0)
                (begin
                  (display (string-append "FAIL: " (number->string total) " file(s) need formatting")
                  ) ;display
                  (newline)
                  (exit 1)
                ) ;begin
                (begin
                  (display "OK: all files formatted.")
                  (newline)
                  (exit 0)
                ) ;begin
              ) ;if
            ) ;let*
            (let ((lang (car langs)))
              (display (string-append "=== Checking "
                         (if (eq? lang 'scheme) "Scheme" "C++")
                         " files ==="
                       ) ;string-append
              ) ;display
              (newline)
              (flush-output)
              (loop (cdr langs) (cons (cons lang (check-lang lang cfg)) results))
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; ---- 单文件 / 目录 / 增量（有路径参数）-----------------------------
    ;; 按后缀选语言。cpp 后缀走 clang-format；其余（scheme 及 -e 指定）走 scheme-fmt。
    (define (cpp-extension? extensions)
      (let loop
        ((exts extensions))
        (if (null? exts)
          #f
          (if (or (string=? (car exts) ".cpp")
                (string=? (car exts) ".hpp")
                (string=? (car exts) ".h")
                (string=? (car exts) ".c")
                (string=? (car exts) ".cc")
                (string=? (car exts) ".cxx")
              ) ;or
            #t
            (loop (cdr exts))
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    ;; ---- 主入口 ---------------------------------------------------------
    (define (main)
      (let ((parser (make-fmt-arg-parser)))
        (parser :parse-argv (argv))
        (let* ((help-flag (parser 'help))
               (dry-run (parser 'dry-run))
               (check-flag (parser 'check))
               (extensions (parse-extensions (parser 'extension)))
               (changed-since (parser 'changed-since))
               (cli-excludes (parse-excludes (parser 'exclude)))
               (path-str (first-positional parser))
              ) ;
          (cond (help-flag (display-help) #t)
                ;; 无路径参数：仓库批量 / check（需 gf_fmt.json）。
                ((string=? path-str "")
                 (let ((cfg (catch #t
                              (lambda () (load-fmt-config))
                              (lambda (type info)
                                (let ((e (if (null? info) type (car info))))
                                  (display (string-append "error: gf_fmt.json 解析失败 - "
                                             (if (string? e) e (object->string e))
                                           ) ;string-append
                                  ) ;display
                                  (newline)
                                  (exit 1)
                                ) ;let
                              ) ;lambda
                            ) ;catch
                       ) ;cfg
                      ) ;
                   (if (not cfg)
                     (begin
                       (display "error: 找不到 gf_fmt.json，请在项目根创建它（无路径参数时必需）。"
                       ) ;display
                       (newline)
                       (exit 1)
                     ) ;begin
                     (if check-flag (run-repo-check cfg) (run-repo-format cfg))
                   ) ;if
                 ) ;let
                ) ;
                ;; 有路径参数 + changed-since：Scheme 增量。
                (changed-since (let ((excludes (append cli-excludes (scheme-config-excludes))))
                                 (format-changed-since changed-since path-str extensions excludes dry-run)
                               ) ;let
                ) ;changed-since
                ;; 单文件。
                ((path-file? (path path-str))
                 (let ((excludes (append cli-excludes (scheme-config-excludes))))
                   (if (cpp-extension? (list (path-suffix (path path-str))))
                     (begin
                       (display "error: 单个 C++ 文件格式化请用 clang-format 直接处理。")
                       (newline)
                       (exit 1)
                     ) ;begin
                     (format-single-file path-str dry-run excludes)
                   ) ;if
                 ) ;let
                ) ;
                ;; 目录递归。
                ((path-dir? (path path-str))
                 (let ((excludes (append cli-excludes (scheme-config-excludes))))
                   (call-with-values (lambda () (format-directory path-str extensions excludes dry-run))
                     (lambda (total updated cached)
                       (display (string-append "Total files formatted: "
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
                 ) ;let
                ) ;
                (else (display (string-append "错误: 路径不存在 - " path-str))
                  (newline)
                  (exit 1)
                ) ;else
          ) ;cond
        ) ;let*
      ) ;let
    ) ;define

    ;; 单文件/目录模式下，从 gf_fmt.json 读 scheme.exclude 作为项目级排除
    ;; （配置不存在时降级为 '()，单文件模式不强制要求配置）。
    (define (scheme-config-excludes)
      (let ((cfg (catch #t (lambda () (load-fmt-config)) (lambda (type info) #f))))
        (if cfg (lang-excludes 'scheme cfg) '())
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
