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

(define-library (liii path fs)
  ;; 谓词
  (export path-dir? path-file? path-exists?)
  ;; 读操作
  (export path-getsize path-read-text path-read-lines path-read-bytes)
  ;; 写操作
  (export path-write-text path-write-bytes path-append-text path-touch)
  ;; 目录操作
  (export path-list path-list-path path-rmdir path-mkdir)
  ;; 文件管理
  (export path-copy path-copy-into path-unlink path-rename)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii os)
    (liii vector)
    (liii path record)
    (liii path pure)
  ) ;import
  (begin

    (define (path-copy source target)
      (let ((src (path->string source)) (dst (path->string target)))
        (if (not (file-exists? src))
          (file-not-found-error (string-append "No such file or directory: '" src "'"))
          (g_path-copy src dst)
        ) ;if
      ) ;let
    ) ;define

    (define (path-copy-into source target-dir)
      (let ((filename (path-name (path source))))
        (path-copy source (path->string (path-join (path target-dir) (path filename))))
      ) ;let
    ) ;define

    (define (path-dir? p)
      (isdir (path->string p))
    ) ;define

    (define (path-file? p)
      (g_isfile (path->string p))
    ) ;define

    (define (path-exists? p)
      (file-exists? (path->string p))
    ) ;define

    (define (path-getsize p)
      (let ((s (path->string p)))
        (if (not (file-exists? s))
          (file-not-found-error (string-append "No such file or directory: '" s "'"))
          (g_path-getsize s)
        ) ;if
      ) ;let
    ) ;define

    (define (path-read-text p)
      (let ((s (path->string p)))
        (cond ((not (file-exists? s))
               (file-not-found-error (string-append "No such file or directory: '" s "'"))
              ) ;
              ((path-dir? s) (file-error (string-append "Is a directory: '" s "'")))
              (else (g_path-read-text s))
        ) ;cond
      ) ;let
    ) ;define

    (define* (path-read-lines p (keepends #f) (offset 0) (limit #f))
      (if (not (or (string? p) (path? p)))
        (type-error "path-read-lines: path must be string or path")
      ) ;if
      (if (not (boolean? keepends))
        (type-error "path-read-lines: keepends must be boolean")
      ) ;if
      (if (not (exact-integer? offset))
        (type-error "path-read-lines: offset must be exact integer")
      ) ;if
      (if (not (or (not limit) (exact-integer? limit)))
        (type-error "path-read-lines: limit must be exact integer or #f")
      ) ;if
      (if (< offset 0) (value-error "path-read-lines: offset must be non-negative"))
      (if (and limit (<= limit 0))
        (value-error "path-read-lines: limit must be a positive integer")
      ) ;if
      (let ((s (path->string p)))
        (cond ((not (file-exists? s))
               (file-not-found-error (string-append "No such file or directory: '" s "'"))
              ) ;
              ((path-dir? s) (file-error (string-append "Is a directory: '" s "'")))
              (else (g_path-read-lines s keepends offset (or limit -1)))
        ) ;cond
      ) ;let
    ) ;define*

    (define (path-read-bytes p)
      (let ((s (path->string p)))
        (cond ((not (file-exists? s))
               (file-not-found-error (string-append "No such file or directory: '" s "'"))
              ) ;
              ((path-dir? s) (file-error (string-append "Is a directory: '" s "'")))
              (else (g_path-read-bytes s))
        ) ;cond
      ) ;let
    ) ;define

    (define (path-write-text p content)
      (if (not (string? content))
        (type-error "path-write-text: content must be string")
        (g_path-write-text (path->string p) content)
      ) ;if
    ) ;define

    (define (path-write-bytes p data)
      (if (not (byte-vector? data))
        (type-error "path-write-bytes: data must be bytevector")
        (g_path-write-bytes (path->string p) data)
      ) ;if
    ) ;define

    (define (path-append-text p content)
      (if (not (string? content))
        (type-error "path-append-text: content must be string")
        (g_path-append-text (path->string p) content)
      ) ;if
    ) ;define

    (define (path-touch p)
      (g_path-touch (path->string p))
    ) ;define

    (define (path-list p)
      (listdir (path->string p))
    ) ;define

    (define (path-list-path p)
      (let ((base (path->string p)))
        (let ((entries (listdir base)))
          (vector-map (lambda (entry) (path-join base entry)) entries)
        ) ;let
      ) ;let
    ) ;define

    (define (path-rmdir p)
      (rmdir (path->string p))
    ) ;define

    (define* (path-unlink p (missing-ok #f))
      (let ((s (path->string p)))
        (cond ((file-exists? s) (remove s))
              (missing-ok #t)
              (else (error 'file-not-found-error (string-append "File not found: " s)))
        ) ;cond
      ) ;let
    ) ;define*

    (define (path-rename src dst)
      (rename (path->string src) (path->string dst))
    ) ;define

    (define* (path-mkdir p (parents #f) (exist-ok #f))
      (define (ensure-one s)
        (if (file-exists? s) #t (mkdir s))
      ) ;define
      (let ((s (path->string p)))
        (cond ((and (not exist-ok) (file-exists? s))
               (file-exists-error (string-append "File exists: '" s "'"))
              ) ;
              ((not parents)
               (let* ((path-s (path s)) (parent (path-parent path-s)))
                 (if (and (not (path=? parent path-s)) (not (path-dir? parent)))
                   (file-not-found-error (string-append "Parent directory not found: '" (path->string parent) "'")
                   ) ;file-not-found-error
                   (ensure-one s)
                 ) ;if
               ) ;let*
              ) ;
              (else
                (let loop
                  ((rest (reverse (vector->list (path-parents (path s))))))
                  (if (null? rest)
                    (ensure-one s)
                    (begin
                      (ensure-one (path->string (car rest)))
                      (loop (cdr rest))
                    ) ;begin
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define*

  ) ;begin
) ;define-library
