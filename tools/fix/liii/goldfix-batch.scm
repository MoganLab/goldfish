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

(define-library (liii goldfix-batch)
  (export fix-file-dry-run fix-file-core fix-file fix-file-list
    file-extension-match? fix-changed-since fix-directory
  ) ;export
  (import (liii base)
    (liii path)
    (liii string)
    (liii os)
    (srfi srfi-13)
    (liii goldfix-cache)
    (liii goldtool-changed)
  ) ;import

  (begin

    (define (fix-file-dry-run path-str repair-source-fn)
      (let ((source (path-read-text (path path-str))))
        (display (repair-source-fn source))
        (newline)
      ) ;let
    ) ;define

    ;; 返回值区分四种结果：
    ;;   'cached   命中缓存，跳过
    ;;   'updated  实际改动了文件（写盘）
    ;;   'balanced 未改动——括号本就平衡、缩进正常，无需修正
    ;;   'skipped  未改动——缩进不可信，被守卫拦截（已打印"请先 gf fmt"）
    (define* (fix-file-core path-str repair-source*-fn (use-cache? #t))
      (if (and use-cache? (fix-cache-hit? path-str))
        'cached
        (let* ((p (path path-str)) (source (path-read-text p)))
          (call-with-values (lambda () (repair-source*-fn source))
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

    (define* (fix-file path-str repair-source*-fn (use-cache? #t))
      (fix-file-core path-str repair-source*-fn use-cache?)
    ) ;define*

    (define (fix-file-list files dry-run repair-source-fn repair-source*-fn)
      (let loop
        ((remaining files) (total 0) (updated 0) (cached 0))
        (if (null? remaining)
          (values total updated cached)
          (let ((file (car remaining)))
            (if dry-run
              (begin
                (display (string-append "Previewing: " file))
                (newline)
                (fix-file-dry-run file repair-source-fn)
                (loop (cdr remaining) (+ total 1) updated cached)
              ) ;begin
              (let ((result (fix-file file repair-source*-fn #t)))
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

    (define (fix-changed-since since path-str dry-run extensions
              repair-source-fn repair-source*-fn
            ) ;fix-changed-since
      (let ((scope (if (string=? path-str "") #f path-str)))
        (cond
         ((and scope (not (or (path-file? (path scope)) (path-dir? (path scope)))))
          (display (string-append "错误: 路径不存在 - " scope))
          (newline)
          (exit 1)
         ) ;
         (else
           (let ((files (if scope
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
               (call-with-values (lambda () (fix-file-list files dry-run repair-source-fn repair-source*-fn))
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

    (define (fix-directory-entry entry extensions repair-source*-fn loop i total
              updated cached
            ) ;fix-directory-entry
      (cond
       ((path-file? entry)
        (let ((entry-str (path->string entry)))
          (if (file-extension-match? entry-str extensions)
            (let ((result (fix-file-core entry-str repair-source*-fn)))
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
        (call-with-values (lambda () (fix-directory (path->string entry) extensions repair-source*-fn))
          (lambda (sub-total sub-updated sub-cached)
            (loop (+ i 1) (+ total sub-total) (+ updated sub-updated) (+ cached sub-cached))
          ) ;lambda
        ) ;call-with-values
       ) ;
       (else (loop (+ i 1) total updated cached))
      ) ;cond
    ) ;define

    (define (fix-directory dir-path extensions repair-source*-fn)
      (let ((entries (path-list-path (path dir-path))))
        (let loop
          ((i 0) (total 0) (updated 0) (cached 0))
          (if (>= i (vector-length entries))
            (values total updated cached)
            (fix-directory-entry (vector-ref entries i)
              extensions
              repair-source*-fn
              loop
              i
              total
              updated
              cached
            ) ;fix-directory-entry
          ) ;if
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
