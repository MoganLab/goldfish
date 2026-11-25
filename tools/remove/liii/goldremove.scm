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

;; gf remove：gf get -g 的逆操作，按短包名卸载用户级依赖：
;; 删 site-lisp 安装目录（本地开发包只清注册）、清 depends/lock、注销工具绑定。
;; 与 gf get 对称，第一版只做单包用户级操作（-g 必填），不提供 rm 别名。

(define-library (liii goldremove)
  (import (scheme base)
    (scheme process-context)
    (scheme write)
    (liii error)
    (liii string)
    (liii gfproject remove)
  ) ;import
  (export main parse-remove-args display-remove-help)
  (begin

    (define (display-remove-help)
      (display "Usage: gf remove -g <name@org>\n")
      (display "\n")
      (display "Remove a user-level package and unregister its gf subcommands.\n")
      (display "\n")
      (display "Options:\n")
      (display "  -g               Remove from user-level ~/.gf (required)\n")
      (display "  -h, --help       Display this help\n")
      (display "\n")
      (display "Remote packages: the installed site-lisp directory is deleted.\n")
      (display "Local development packages: only the registration is removed;\n")
      (display "the working copy is kept untouched.\n")
    ) ;define

    ;; 解析参数：返回 '(help) / '(remove id) / '(error msg)。
    (define (parse-remove-args args)
      (let loop
        ((rest args) (positional '()) (global? #f))
        (cond
         ((null? rest)
          (cond ((not global?) (list 'error "gf remove: -g (user-level) is required"))
                ((null? positional) (list 'error "gf remove: missing package id"))
                ((> (length positional) 1)
                 (list 'error "gf remove: expected exactly one package id")
                ) ;
                (else (list 'remove (car positional)))
          ) ;cond
         ) ;
         ((or (string=? (car rest) "-h") (string=? (car rest) "--help")) '(help))
         ((string=? (car rest) "-g") (loop (cdr rest) positional #t))
         ((string-starts? (car rest) "-")
          (list 'error (string-append "gf remove: unknown option " (car rest)))
         ) ;
         (else (loop (cdr rest) (cons (car rest) positional) global?))
        ) ;cond
      ) ;let
    ) ;define

    (define (main)
      (let* ((argv (command-line))
             (tool-args
               (if (and (pair? argv) (pair? (cdr argv))) (cddr argv) '())
             ) ;tool-args
             (parsed (parse-remove-args tool-args))
            ) ;
        (case (car parsed)
              ((help) (display-remove-help) 0)
              ((error)
               (display (string-append (cadr parsed) "\n") (current-error-port))
               (display-remove-help)
               1
              ) ;
              ((remove)
               (catch #t
                 (lambda () (gfproject-remove-package (cadr parsed)))
                 (lambda (type info)
                   (let ((msg (catch-error->string type info)))
                     (display (string-append (if (string-starts? msg "gf remove:") "" "gf remove: ") msg "\n")
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
