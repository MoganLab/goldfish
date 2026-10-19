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
  (export main fix-string repair-source repair-source* repair-parentheses
    parentheses-balanced?
  ) ;export
  (import (liii base)
    (liii sys)
    (liii path)
    (liii string)
    (liii os)
    (liii goldfix repair)
    (liii goldfix record)
    (liii goldfix cache)
    (liii goldfix cli)
    (liii goldfix batch)
  ) ;import

  (begin

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

    (define (handle-file-target path-str dry-run)
      (if dry-run
        (fix-file-dry-run path-str repair-source)
        (let ((result (fix-file path-str repair-source* #t)))
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
    ) ;define

    (define (handle-dir-target path-str dry-run extensions)
      (if dry-run
        (begin
          (display "错误: --dry-run 选项仅支持单个文件")
          (newline)
          (exit 1)
        ) ;begin
        (call-with-values (lambda () (fix-directory path-str extensions repair-source*))
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
                (changed-since (fix-changed-since changed-since path-str dry-run
                                 extensions repair-source repair-source*
                               ) ;fix-changed-since
                ) ;changed-since
                ((string=? path-str "") (display-help) #t)
                ((path-file? (path path-str)) (handle-file-target path-str dry-run))
                ((path-dir? (path path-str)) (handle-dir-target path-str dry-run extensions))
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
