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

(define-library (liii goldfmt args)
  (import (liii base) (liii string) (liii argparse) (liii goldfmt lang))
  (export normalize-extension token->extensions file-extension-match?
    parse-extensions parse-excludes make-fmt-arg-parser normalize-jobs
    parse-fmt-jobs first-positional display-help extension-option-explicit?
  ) ;export
  (begin

    (define (normalize-extension ext)
      (if (and (> (string-length ext) 0) (char=? (string-ref ext 0) #\.))
        ext
        (string-append "." ext)
      ) ;if
    ) ;define

    ;; 按 -e token 转成后缀列表：若是语言名（cpp/scheme/...）展开为该语言后缀表，
    ;; 否则按后缀处理（补点）。使 -e cpp 涵盖 .cpp/.hpp/.h/.c/.cc/.cxx 全部。
    (define (token->extensions token)
      (let ((lang-exts (extensions-for-lang-name token)))
        (if lang-exts lang-exts (list (normalize-extension token)))
      ) ;let
    ) ;define

    (define (file-extension-match? filename extensions)
      (let loop
        ((exts extensions))
        (if (null? exts)
          #f
          (if (string-ends? filename (car exts)) #t (loop (cdr exts)))
        ) ;if
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
                                            (skip-value-options "-m" "--mode"
                                              "-I" "-A")
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
        (parser
          :add-argument
          '((name . "extension")
            (short . "e")
            (type . string)
            (default . "scm"))
        ) ;parser
        (parser :add-argument '((name . "changed-since") (type . string)))
        (parser :add-argument '((name . "exclude") (type . string)))
        (parser
          :add-argument
          '((name . "jobs") (short . "j") (type . number) (default . 0))
        ) ;parser
        parser
      ) ;let
    ) ;define

    ;; jobs 原始值归一化：不小于 1 的整数；0 表示自动（由主入口解析为
    ;; (go-worker-count)，即 CPU 硬件核数）。非数字（含解析失败）也归 0。
    (define (normalize-jobs j)
      (if (and (number? j) (> j 0)) (max 1 (exact (floor j))) 0)
    ) ;define

    ;; 解析 -j/--jobs：返回不小于 1 的整数；0 表示自动。非法输入（如 -j abc）
    ;; 在此处不报错、回退自动模式（供建模为独立函数供单元测试）。
    (define (parse-fmt-jobs args)
      (normalize-jobs
        (catch #t
          (lambda ()
            (let ((parser (make-fmt-arg-parser)))
              (parser :parse-argv args)
              (parser 'jobs)
            ) ;let
          ) ;lambda
          (lambda (type info) #f)
        ) ;catch
      ) ;normalize-jobs
    ) ;define

    (define (first-positional parser)
      (let ((positionals (parser :positionals)))
        (if (null? positionals) "" (car positionals))
      ) ;let
    ) ;define

    (define (display-help)
      (display "Usage: gf fmt [options] [path]")
      (newline)
      (newline)
      (display "Options:")
      (newline)
      (display "  -h, --help       显示此帮助文档")
      (newline)
      (display "      --check      非破坏性检查：发现未格式化文件则退出码 1（可针对整个仓库、单目录或单文件）"
      ) ;display
      (newline)
      (display "      --dry-run    预览模式（不写回文件；目录路径不支持）"
      ) ;display
      (newline)
      (display "  -e, --extension EXT    按语言名或后缀指定：-e cpp 涵盖 .cpp/.hpp/.h/.c/.cc/.cxx；-e scheme 涵盖 .scm；-e stem 涵盖 .stem；也可直接写后缀 -e scm,sld"
      ) ;display
      (newline)
      (display "      --changed-since REV    仅格式化自 REV 以来变更的文件（按 -e 过滤；未指定 -e 时包含所有语言）"
      ) ;display
      (newline)
      (display "      --exclude PATTERN    跳过匹配的文件（路径后缀匹配，逗号分隔多个）"
      ) ;display
      (newline)
      (display "  -j, --jobs NUM   并发格式化的协程数（默认 0 = 自动，取 CPU 核数；1 = 单协程；受 GOLDFISH_GO_WORKERS 环境变量约束）"
      ) ;display
      (newline)
      (newline)
      (display "Arguments:")
      (newline)
      (display "  path    要格式化的文件或目录路径（可选）")
      (newline)
      (display "          无 path 时：读 gf_fmt.json 仓库批量格式化所有语言（scheme + cpp + stem）"
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
      (display "  gf fmt --check /path/to/dir  CI 非破坏性检查指定目录")
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
      (display "  gf fmt --changed-since=HEAD  格式化自 HEAD 以来变更的所有文件"
      ) ;display
      (newline)
    ) ;define

    (define (extension-option-explicit? args)
      (let loop
        ((as args))
        (if (null? as)
          #f
          (let ((a (car as)))
            (if (or (string=? a "-e")
                  (string=? a "--extension")
                  (string-starts? a "-e=")
                  (string-starts? a "--extension=")
                ) ;or
              #t
              (loop (cdr as))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
