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
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; gf get：把带 gfproject.json 的完整多 domain 项目安装为用户级依赖，
;; 并自动编辑 ~/.gf/gfproject.json 注册其 gf 子命令。
;; 第一版只做单包用户级操作，不做遍历、--sync 与项目级 depends。

(define-library (liii goldget)
  (import (scheme base)
    (scheme process-context)
    (scheme write)
    (liii error)
    (liii string)
    (liii gfproject get install)
  ) ;import
  (export main parse-get-args display-get-help)
  (begin

    (define (display-get-help)
      (display "Usage: gf get -g <source> [options]\n")
      (display "\n")
      (display "Install a package and register its gf subcommands globally.\n")
      (display "\n")
      (display "Sources:\n")
      (display "  <url>            Repository URL (first installation)\n")
      (display "  <path>           Local project directory (development mode)\n")
      (display "  <name@org>       Registered package id (reinstall/update)\n")
      (display "\n")
      (display "Options:\n")
      (display "  -g               Install into user-level ~/.gf (required)\n")
      (display "  --from <source>  Repository for a package id (first installation)\n"
      ) ;display
      (display "  --force          Replace conflicting source or tool ownership\n")
      (display "  -h, --help       Display this help\n")
    ) ;define

    ;; 解析参数：返回 '(help) / '(install arg from force?) / '(error msg)。
    (define (parse-get-args args)
      (let loop
        ((rest args) (positional '()) (global? #f) (from #f) (force? #f))
        (cond
         ((null? rest)
          (cond ((not global?) (list 'error "gf get: -g (user-level) is required"))
                ((null? positional) (list 'error "gf get: missing package source"))
                ((> (length positional) 1)
                 (list 'error "gf get: expected exactly one package source")
                ) ;
                (else (list 'install (car positional) from force?))
          ) ;cond
         ) ;
         ((or (string=? (car rest) "-h") (string=? (car rest) "--help")) '(help))
         ((string=? (car rest) "-g") (loop (cdr rest) positional #t from force?))
         ((string=? (car rest) "--force") (loop (cdr rest) positional global? from #t))
         ((string=? (car rest) "--from")
          (if (null? (cdr rest))
            '(error "gf get: --from requires a source argument")
            (loop (cddr rest) positional global? (cadr rest) force?)
          ) ;if
         ) ;
         ((string-starts? (car rest) "-")
          (list 'error (string-append "gf get: unknown option " (car rest)))
         ) ;
         (else (loop (cdr rest) (cons (car rest) positional) global? from force?))
        ) ;cond
      ) ;let
    ) ;define

    (define (main)
      (let* ((argv (command-line))
             (tool-args
               (if (and (pair? argv) (pair? (cdr argv))) (cddr argv) '())
             ) ;tool-args
             (parsed (parse-get-args tool-args))
            ) ;
        (case (car parsed)
              ((help) (display-get-help) 0)
              ((error)
               (display (string-append (cadr parsed) "\n") (current-error-port))
               (display-get-help)
               1
              ) ;
              ((install)
               (catch #t
                 (lambda () (get-install (cadr parsed) (caddr parsed) (cadddr parsed)))
                 (lambda (type info)
                   (let ((msg (catch-error->string type info)))
                     (display (string-append (if (string-starts? msg "gf get:") "" "gf get: ") msg "\n")
                       (current-error-port)
                     ) ;display
                   ) ;let
                   1
                 ) ;lambda
               ) ;catch
              ) ;
              (else 1)
        ) ;case
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
