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

(define-library (liii goldfix cli)
  (export display-help make-fix-arg-parser first-positional normalize-extension
    parse-extensions
  ) ;export
  (import (liii base) (liii string) (liii argparse))

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

    (define (make-fix-arg-parser)
      (let ((parser (make-argument-parser '((command . "fix")
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
        (parser
          :add-argument
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

  ) ;begin
) ;define-library
