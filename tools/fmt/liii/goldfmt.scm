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

    ;; 把单个 -e token 转成后缀列表：若是语言名（cpp/scheme/...）展开为该语言后缀表，
    ;; 否则按后缀处理（补点）。使 -e cpp 涵盖 .cpp/.hpp/.h/.c/.cc/.cxx 全部。
    (define (token->extensions token)
      (let ((lang-exts (extensions-for-lang-name token)))
        (if lang-exts lang-exts (list (normalize-extension token)))
      ) ;let
    ) ;define

    (define (parse-extensions raw)
      (let loop
        ((tokens (string-split raw ",")) (acc '()))
        (if (null? tokens)
          (let dedup
            ((es (reverse acc)) (seen '()))
            (if (null? es)
              seen
              (dedup (cdr es) (if (member (car es) seen) seen (cons (car es) seen)))
            ) ;if
          ) ;let
          (loop (cdr tokens) (append (token->extensions (car tokens)) acc))
        ) ;if
      ) ;let
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
      (display "  -e, --extension EXT    按语言名或后缀指定：-e cpp 涵盖 .cpp/.hpp/.h/.c/.cc/.cxx；-e scheme 涵盖 .scm；也可直接写后缀 -e scm,sld"
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

    ;; ---- 仓库批量 / check（无路径参数，读 gf_fmt.json）------------------
    ;; 通过语言注册表派发：遍历 (lang-list)，对每个 handler 取 collect/format-files/
    ;; check-file 方法调用。新增语言只需注册 handler，此处零改动。

    (define (flush-output)
      (flush-output-port (current-output-port))
    ) ;define

    ;; 仓库批量格式化：逐语言收集 + 格式化。
    (define (run-repo-format cfg)
      (let loop
        ((handlers (lang-list)))
        (if (null? handlers)
          (begin
            (display "Done.")
            (newline)
            #t
          ) ;begin
          (let* ((handler (car handlers))
                 (label (lang-label handler))
                 (collect (lang-ref handler 'collect))
                 (format-files (lang-ref handler 'format-files))
                 (files (collect cfg))
                 (stats (format-files files cfg))
                ) ;
            (display (string-append "=== Formatting " label " files ==="))
            (newline)
            (flush-output)
            (display (string-append "Total "
                       label
                       " files formatted: "
                       (number->string (car stats))
                       ", Files updated: "
                       (number->string (cadr stats))
                       ", Files unchanged: "
                       (number->string (caddr stats))
                     ) ;string-append
            ) ;display
            (newline)
            (newline)
            (loop (cdr handlers))
          ) ;let*
        ) ;if
      ) ;let
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
    (define (check-lang handler cfg)
      (let* ((label (lang-label handler))
             (collect (lang-ref handler 'collect))
             (check-file (lang-ref handler 'check-file))
             (files (collect cfg))
            ) ;
        (let loop
          ((fs files) (bad '()))
          (if (null? fs)
            (reverse bad)
            (let ((f (car fs)))
              (loop (cdr fs) (if (check-file f cfg) bad (cons f bad)))
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    ;; 仓库级非破坏检查：逐语言收集 + 逐文件检查，汇总 offenders。
    ;; 任一文件未格式化则退出码 1（供 CI）。Windows 无 sh 时跳过并退出 0（CI 在 Debian 跑）。
    (define (run-repo-check cfg)
      (if (os-windows?)
        (begin
          (display "fmt --check: skipped on Windows (requires sh).")
          (newline)
          (exit 0)
        ) ;begin
        (let loop
          ((handlers (lang-list)) (results '()))
          (if (null? handlers)
            (let ((total (let sum
                           ((rs results) (n 0))
                           (if (null? rs) n (sum (cdr rs) (+ n (length (cdar rs)))))
                         ) ;let
                  ) ;total
                 ) ;
              (newline)
              (for-each (lambda (r) (print-offenders (lang-label (car r)) (cdr r)))
                (reverse results)
              ) ;for-each
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
            ) ;let
            (let* ((handler (car handlers)) (label (lang-label handler)))
              (display (string-append "=== Checking " label " files ==="))
              (newline)
              (flush-output)
              (loop (cdr handlers) (cons (cons handler (check-lang handler cfg)) results))
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; ---- 单文件 / 目录 / 增量（有路径参数）-----------------------------
    ;; 按后缀查语言注册表派发（lang-for-extension / lang-for-extensions），
    ;; 主入口不硬编码任何语言；找不到匹配 handler 时默认走 scheme。
    (define (scheme-handler-of)
      (lang-for-extension ".scm")
    ) ;define

    ;; 单文件：按文件后缀查 handler，调其 format-file；无匹配则用 scheme handler。
    (define (dispatch-format-file path-str dry-run excludes)
      (let* ((ext (path-suffix (path path-str)))
             (handler (or (lang-for-extension ext) (scheme-handler-of)))
             (format-file (lang-ref handler 'format-file))
            ) ;
        (format-file path-str dry-run excludes)
      ) ;let*
    ) ;define

    ;; 按 -e 后缀选目录模式的语言 handler：
    ;;   后缀全部属于同一语言 → 该 handler；无匹配 → scheme；混合多语言 → 报错退出。
    (define (directory-handler-for extensions)
      (let ((matched (lang-for-extensions extensions)))
        (cond ((null? matched) (scheme-handler-of))
              ((null? (cdr matched)) (car matched))
              (else (display "错误: 目录格式化不支持混合语言后缀，请用单一语言的 -e"
                    ) ;display
                (newline)
                (exit 1)
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; 目录：选语言 handler，调其 format-directory（返回 (total updated unchanged) 列表），统一打印统计行。
    (define (dispatch-format-directory dir extensions excludes dry-run)
      (let* ((handler (directory-handler-for extensions))
             (format-directory (lang-ref handler 'format-directory))
             (stats (format-directory dir extensions excludes dry-run))
            ) ;
        (display (string-append "Total files formatted: "
                   (number->string (car stats))
                   ", Files updated: "
                   (number->string (cadr stats))
                   ", Files unchanged: "
                   (number->string (caddr stats))
                 ) ;string-append
        ) ;display
        (newline)
        #t
      ) ;let*
    ) ;define

    ;; 单文件/目录模式下，从 gf_fmt.json 读 scheme.exclude 作为项目级排除
    ;; （配置不存在时降级为 '()，单文件模式不强制要求配置）。
    (define (scheme-config-excludes)
      (let ((cfg (catch #t (lambda () (load-fmt-config)) (lambda (type info) #f))))
        (if cfg (lang-excludes 'scheme cfg) '())
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
                ;; changed-since 优先：即使无路径参数也走 Scheme 增量，而非仓库批量。
                (changed-since (let ((excludes (append cli-excludes (scheme-config-excludes))))
                                 (format-changed-since changed-since path-str extensions excludes dry-run)
                               ) ;let
                ) ;changed-since
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
                ;; 单文件。按后缀查注册表派发到对应语言。
                ((path-file? (path path-str))
                 (let ((excludes (append cli-excludes (scheme-config-excludes))))
                   (dispatch-format-file path-str dry-run excludes)
                 ) ;let
                ) ;
                ;; 目录递归。按 -e 后缀查注册表派发到对应语言。
                ((path-dir? (path path-str))
                 (let ((excludes (append cli-excludes (scheme-config-excludes))))
                   (dispatch-format-directory path-str extensions excludes dry-run)
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

  ) ;begin
) ;define-library
