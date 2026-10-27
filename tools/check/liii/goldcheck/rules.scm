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

(define-library (liii goldcheck rules)
  (import (scheme base)
    (scheme file)
    (scheme write)
    (liii base)
    (liii path)
    (liii string)
    (liii tree)
    (liii goldcheck path)
  ) ;import
  (export check-file-lines check-tree-depth check-no-crlf format-violation
    display-help
  ) ;export
  (begin

    (define (display-help)
      (display "Usage: gf check [options] [PATH...]")
      (newline)
      (newline)
      (display "对 Scheme 代码执行静态度量检查（只读，不修改任何文件）。"
      ) ;display
      (newline)
      (newline)
      (display "Options:")
      (newline)
      (display "  -h, --help             显示此帮助信息")
      (newline)
      (display "      --changed-since REV    仅检查自 REV 以来变更的 .scm 文件"
      ) ;display
      (newline)
      (display "      --only RULE            只检查指定规则（如 layer-order）"
      ) ;display
      (newline)
      (newline)
      (display "Rules:")
      (newline)
      (display "  layer-order            域间依赖与域内层序约束")
      (newline)
      (display "  max-file-lines         单文件行数限制（默认 <= 400 行，含空行与注释）"
      ) ;display
      (newline)
      (display "  max-tree-depth         语法树嵌套深度限制（默认 <= 15 层顶级表达式嵌套）"
      ) ;display
      (newline)
      (display "  define-record-type     <xxx> 记录类型句法标识不得导出，禁止占位定义"
      ) ;display
      (newline)
      (display "  no-crlf                换行符统一为 LF，发现 CRLF 报错（按 suffix/path 限定范围）"
      ) ;display
      (newline)
      (display "  no-glue                glue 仅限唯一 define-library 封装，并提供同名公开接口"
      ) ;display
      (newline)
      (display "  type-hint              hint 库结构与签名校验（仅检查 goldfish/hint/ 下的库）"
      ) ;display
      (newline)
      (newline)
      (display "Arguments:")
      (newline)
      (display "  PATH...                要检查的文件或目录路径（可选）"
      ) ;display
      (newline)
      (display "                         无 PATH 且无 --changed-since 时，按 gf_check.json 对全仓检查"
      ) ;display
      (newline)
      (newline)
      (display "Examples:")
      (newline)
      (display "  gf check                               全仓静态检查")
      (newline)
      (display "  gf check --changed-since=main          仅检查自 main 以来变更的文件"
      ) ;display
      (newline)
      (display "  gf check goldfish/liii/os.scm          检查单个文件")
      (newline)
      (display "  gf check goldfish/liii/                递归检查目录下所有 .scm 文件"
      ) ;display
      (newline)
      (newline)
      (display "Exit Codes:")
      (newline)
      (display "  0                      全部检查通过")
      (newline)
      (display "  1                      存在违规项或配置错误")
      (newline)
    ) ;define

    (define (check-file-lines file-path limit)
      "统计文件的物理行数（含空行与注释）。返回 (values count violation-or-#f)"
      (let* ((rel-path (relativize-path file-path)) (port (open-input-file file-path)))
        (dynamic-wind (lambda () #f)
          (lambda ()
            (let loop
              ((count 0))
              (let ((line (read-line port)))
                (if (eof-object? line)
                  (values count
                    (if (> count limit) (list 'max-file-lines rel-path count limit) #f)
                  ) ;values
                  (loop (+ count 1))
                ) ;if
              ) ;let
            ) ;let
          ) ;lambda
          (lambda () (close-input-port port))
        ) ;dynamic-wind
      ) ;let*
    ) ;define

    (define (safe-read port)
      (catch #t (lambda () (read port)) (lambda (err-type info) (eof-object)))
    ) ;define

    (define (read-tree-depth-loop port max-d max-l)
      (let ((expr (safe-read port)))
        (if (eof-object? expr)
          (values max-d max-l)
          (let* ((d (tree-depth expr))
                 (line (if (pair? expr) (or (pair-line-number expr) 1) 1))
                ) ;
            (if (> d max-d)
              (read-tree-depth-loop port d line)
              (read-tree-depth-loop port max-d max-l)
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (check-tree-depth file-path limit)
      "逐顶级表达式读取并度量树嵌套深度。返回 (values max-depth max-line violation-or-#f)"
      (let* ((rel-path (relativize-path file-path))
             (port (open-input-file file-path))
             (max-d 0)
             (max-l 1)
            ) ;
        (dynamic-wind (lambda () #f)
          (lambda ()
            (call-with-values (lambda () (read-tree-depth-loop port 0 1))
              (lambda (d l) (set! max-d d) (set! max-l l))
            ) ;call-with-values
          ) ;lambda
          (lambda () (close-input-port port))
        ) ;dynamic-wind
        (values max-d
          max-l
          (if (> max-d limit) (list 'max-tree-depth rel-path max-l max-d limit) #f)
        ) ;values
      ) ;let*
    ) ;define

    (define (check-no-crlf file-path)
      "检测文件是否包含 CRLF 换行符，仓库统一使用 LF。返回 violation-or-#f，\nviolation 为 (list 'no-crlf file line)，line 为首个 CRLF 所在行号。"
      (let* ((rel-path (relativize-path file-path))
             (bytes (path-read-bytes (path file-path)))
             (len (bytevector-length bytes))
            ) ;
        (let loop
          ((i 0) (line 1))
          (if (>= i len)
            #f
            (let ((b (bytevector-u8-ref bytes i)))
              (cond
               ((and (= b 13) (< (+ i 1) len) (= (bytevector-u8-ref bytes (+ i 1)) 10))
                (list 'no-crlf rel-path line)
               ) ;
               ((= b 10) (loop (+ i 1) (+ line 1)))
               (else (loop (+ i 1) line))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (format-violation viol)
      (let ((rule (car viol)))
        (cond
         ((eq? rule 'max-file-lines)
          (let ((file (cadr viol)) (lines (caddr viol)) (limit (cadddr viol)))
            (string-append "  "
              file
              ": max-file-lines: "
              (number->string lines)
              " (limit "
              (number->string limit)
              ")"
            ) ;string-append
          ) ;let
         ) ;
         ((eq? rule 'layer-order)
          (let ((file (cadr viol)) (detail (caddr viol)))
            (string-append "  " file ": layer-order: " detail)
          ) ;let
         ) ;
         ((eq? rule 'max-tree-depth)
          (let ((file (cadr viol))
                (line (caddr viol))
                (depth (cadddr viol))
                (limit (list-ref viol 4))
               ) ;
            (string-append "  "
              file
              ":"
              (number->string line)
              ": max-tree-depth: "
              (number->string depth)
              " (limit "
              (number->string limit)
              ")"
            ) ;string-append
          ) ;let
         ) ;
         ((eq? rule 'define-record-type)
          (let ((file (cadr viol)) (line (caddr viol)) (msg (cadddr viol)))
            (string-append "  " file ":" (number->string line) ": define-record-type: " msg)
          ) ;let
         ) ;
         ((eq? rule 'no-crlf)
          (let ((file (cadr viol)) (line (caddr viol)))
            (string-append "  "
              file
              ":"
              (number->string line)
              ": no-crlf: CRLF line ending detected (expected LF)"
            ) ;string-append
          ) ;let
         ) ;
         ((eq? rule 'type-hint)
          (let ((file (cadr viol)) (line (caddr viol)) (msg (cadddr viol)))
            (string-append "  " file ":" (number->string line) ": type-hint: " msg)
          ) ;let
         ) ;
         ((eq? rule 'no-glue)
          (let* ((file (cadr viol))
                 (line (caddr viol))
                 (glue (cadddr viol))
                 (reason (list-ref viol 4))
                 (public (substring (symbol->string glue) 2))
                 (detail (cond ((eq? reason 'outside-library)
                                "must only be used inside its encapsulating define-library"
                               ) ;
                               ((eq? reason 'multiple-libraries)
                                "is encapsulated by multiple define-library forms"
                               ) ;
                               ((eq? reason 'missing-binding) (string-append "missing public binding " public))
                               ((eq? reason 'not-exported)
                                (string-append "public binding " public " is not exported")
                               ) ;
                               ((eq? reason 'exported-glue) "glue binding must not be exported")
                               (else (symbol->string reason))
                         ) ;cond
                 ) ;detail
                ) ;
            (string-append "  "
              file
              ":"
              (number->string line)
              ": no-glue: "
              (symbol->string glue)
              " "
              detail
            ) ;string-append
          ) ;let*
         ) ;
         (else (string-append "  " (object->string viol)))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
