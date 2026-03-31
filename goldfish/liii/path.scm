;
; Copyright (C) 2026 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(define-library (liii path)
  (export
    path path? path-copy
    path-dir? path-file? path-exists?
    path-getsize path-read-text path-read-bytes
    path-write-text path-append-text path-touch
    path-root path-of-drive path-from-parts path-from-env
    path-cwd path-home path-temp-dir
    path-parts path-type path-drive
    path->string path-from-string
    path-name path-stem path-suffix
    path-equals? path=?
    path-absolute? path-relative?
    path-join path-parent
    path-list path-list-path
    path-rmdir path-unlink
  ) ;export
  (import (liii base)
          (liii error)
          (liii os)
          (liii string)
          (scheme base)
  ) ;import
  (begin

    ;;; Path record type
    (define-record-type <path>
      (make-path-record parts type drive)
      path?
      (parts path-record-parts path-record-set-parts!)
      (type path-record-type path-record-set-type!)
      (drive path-record-drive path-record-set-drive!)
    ) ;define-record-type

    (define (vector-copy v)
      (let ((len (vector-length v)))
        (let ((new (make-vector len)))
          (let loop ((i 0))
            (if (< i len)
              (begin
                (vector-set! new i (vector-ref v i))
                (loop (+ i 1))
              ) ;begin
              new
            ) ;if
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    (define (vector-empty? v)
      (= (vector-length v) 0)
    ) ;define

    (define (string-split-vec str sep)
      (let loop ((chars (string->list str))
                 (current '())
                 (result '()))
        (cond
          ((null? chars)
           (list->vector (reverse (cons (list->string (reverse current)) result))))
          ((char=? (car chars) sep)
           (loop (cdr chars) '() (cons (list->string (reverse current)) result)))
          (else
           (loop (cdr chars) (cons (car chars) current) result))
        ) ;cond
      ) ;let
    ) ;define

    ;;; Parse string path into parts
    ;; For absolute paths like "/home/da", the first part is "" to indicate leading /
    (define (parse-path-string s)
      (cond
        ((string-null? s) #("."))
        ((string=? s ".") #("."))
        ((string=? s "/") #("/"))
        (else
         (let ((sep (os-sep)))
           (if (and (> (string-length s) 0) (char=? (string-ref s 0) sep))
             ;; Absolute path: start with empty string part
             (let ((parts (string-split-vec s sep)))
               (if (or (vector-empty? parts)
                       (not (string-null? (vector-ref parts 0))))
                 (vector-append #("" ) parts)
                 parts))
             ;; Relative path
             (string-split-vec s sep))
         ) ;let
        ) ;else
      ) ;cond
    ) ;define

    ;;; Create a path object
    (define (path . args)
      (if (null? args)
        (make-path-record #(".") 'posix "")
        (let ((arg (car args)))
          (cond
            ((string? arg)
             (let ((parts (parse-path-string arg)))
               (make-path-record parts 'posix "")
             ) ;let
            ) ;
            ((path? arg)
             (path-copy arg)
            ) ;
            (else
             (type-error "path: argument must be string or path")
            ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    ;;; Copy a path object
    (define (path-copy p)
      (if (path? p)
        (make-path-record
          (vector-copy (path-record-parts p))
          (path-record-type p)
          (path-record-drive p))
        (type-error "path-copy: argument must be path")
      ) ;if
    ) ;define

    ;;; Get parts as vector
    (define (path-parts p)
      (if (path? p)
        (vector-copy (path-record-parts p))
        (type-error "path-parts: argument must be path")
      ) ;if
    ) ;define

    ;;; Get type ('posix or 'windows)
    (define (path-type p)
      (if (path? p)
        (path-record-type p)
        (type-error "path-type: argument must be path")
      ) ;if
    ) ;define

    ;;; Get drive letter (for Windows paths)
    (define (path-drive p)
      (if (path? p)
        (path-record-drive p)
        (type-error "path-drive: argument must be path")
      ) ;if
    ) ;define

    ;;; Convert path to string
    (define (path->string p)
      (cond
        ((path? p)
         (let ((parts (path-record-parts p))
               (type (path-record-type p))
               (drive (path-record-drive p)))
           (case type
             ((posix)
              (if (vector-empty? parts)
                ""
                (let ((first (vector-ref parts 0)))
                  (if (string-null? first)
                    ;; Absolute path
                    (parts->string parts (string (os-sep)))
                    ;; Relative path
                    (parts->string parts (string (os-sep))))
                ) ;let
              ) ;if
             ) ;
             ((windows)
              (let ((s (parts->string parts "\\")))
                (if (string-null? drive)
                  s
                  (string-append drive ":\\" s)
                ) ;if
              ) ;let
             ) ;
             (else
              (value-error "path->string: unknown type")
             ) ;else
           ) ;case
         ) ;let
        ) ;
        ((string? p)
         p)
        (else
         (type-error "path->string: argument must be path or string")
        ) ;else
      ) ;cond
    ) ;define

    (define (path-from-string s)
      (path s)
    ) ;define

    ;;; Helper: convert parts vector to string
    ;;; For absolute paths, first part is "" or "/" which should result in leading /
    (define (parts->string parts sep)
      (let ((len (vector-length parts)))
        (if (= len 0)
          ""
          (let ((first (vector-ref parts 0)))
            (cond
              ;; Absolute path indicated by empty first part
              ((string-null? first)
               (if (= len 1)
                 sep
                 (let loop ((i 1) (result ""))
                   (if (>= i len)
                     result
                     (let ((part (vector-ref parts i)))
                       (if (string-null? result)
                         (loop (+ i 1) (string-append sep part))
                         (loop (+ i 1) (string-append result sep part))
                       ) ;if
                     ) ;let
                   ) ;if
                 ) ;let
               ) ;if
              ) ;
              ;; Absolute path indicated by "/" as first part (from path-from-parts)
              ((string=? first "/")
               (if (= len 1)
                 sep
                 ;; Join remaining parts with sep, then prepend /
                 (let loop ((i 1) (result ""))
                   (if (>= i len)
                     (string-append sep result)
                     (let ((part (vector-ref parts i)))
                       (if (string-null? result)
                         (loop (+ i 1) part)
                         (loop (+ i 1) (string-append result sep part))
                       ) ;if
                     ) ;let
                   ) ;if
                 ) ;let
               ) ;if
              ) ;
              ;; Relative path
              (else
               (let loop ((i 0) (result ""))
                 (if (>= i len)
                   result
                   (let ((part (vector-ref parts i)))
                     (if (string-null? result)
                       (loop (+ i 1) part)
                       (loop (+ i 1) (string-append result sep part))
                     ) ;if
                   ) ;let
                 ) ;if
               ) ;let
              ) ;else
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;;; Check if two paths are equal
    (define (path-equals? p1 p2)
      (let ((s1 (path->string p1))
            (s2 (path->string p2)))
        (string=? s1 s2)
      ) ;let
    ) ;define

    (define path=? path-equals?)

    ;;; Check if path is absolute
    (define (path-absolute? p)
      (if (path? p)
        (let ((type (path-record-type p))
              (drive (path-record-drive p))
              (parts (path-record-parts p)))
          (case type
            ((windows)
             ;; Windows absolute path has a drive letter
             (not (string-null? drive)))
            ((posix)
             ;; POSIX absolute path starts with empty part (leading /) or is just "/"
             (and (> (vector-length parts) 0)
                  (let ((first (vector-ref parts 0)))
                    (or (string-null? first)
                        (string=? first "/")))))
            (else #f)
          ) ;case
        ) ;let
        (let ((s (path->string p)))
          (cond
            ((os-windows?)
             (and (>= (string-length s) 2)
                  (char=? (string-ref s 1) #\:)))
            (else
             (and (> (string-length s) 0)
                  (char=? (string-ref s 0) (os-sep))))
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    ;;; Check if path is relative
    (define (path-relative? p)
      (not (path-absolute? p))
    ) ;define

    ;;; Get the last component of path (filename)
    (define (path-name p)
      (let ((s (path->string p)))
        ;; Handle special cases: empty string and "." both represent current dir
        (if (or (string-null? s) (string=? s "."))
          ""
          (let ((sep (os-sep)))
            (let loop ((i (- (string-length s) 1)))
              (cond
                ((< i 0) s)
                ((char=? (string-ref s i) sep)
                 (substring s (+ i 1) (string-length s)))
                (else (loop (- i 1)))
              ) ;cond
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;;; Get the stem (filename without extension)
    (define (path-stem p)
      (let ((name (path-name p)))
        (let ((splits (string-split name #\.)))
          (let ((count (length splits)))
            (cond
              ((<= count 1) name)
              ((string=? name ".") "")
              ((string=? name "..") "..")
              ((and (string=? (car splits) "")
                    (= count 2))
               name)
              (else
               ;; Take all parts except the last one and join with "."
               (let loop ((i 0) (result ""))
                 (if (>= i (- count 1))
                   result
                   (let ((part (list-ref splits i)))
                     (if (string-null? result)
                       (loop (+ i 1) part)
                       (loop (+ i 1) (string-append result "." part))
                     ) ;if
                   ) ;let
                 ) ;if
               ) ;let
              ) ;else
            ) ;cond
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    ;;; Get the suffix (file extension)
    (define (path-suffix p)
      (let ((name (path-name p)))
        (let ((splits (string-split name #\.)))
          (let ((count (length splits)))
            (cond
              ((<= count 1) "")
              ((string=? name ".") "")
              ((string=? name "..") "")
              ((and (string=? (car splits) "")
                    (= count 2))
               "")
              (else
               (string-append "." (list-ref splits (- count 1)))
              ) ;else
            ) ;cond
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    ;;; Join paths
    (define (path-join base . segments)
      (let ((sep (string (os-sep))))
        (let loop ((result (path->string base))
                   (rest segments))
          (if (null? rest)
            result
            (let ((part (path->string (car rest))))
              (if (or (string-null? result)
                      (string-ends? result sep))
                (loop (string-append result part) (cdr rest))
                (loop (string-append result sep part) (cdr rest))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;;; Get parent directory
    (define (path-parent p)
      (let ((s (path->string p)))
        (let ((sep (os-sep)))
          ;; First, remove trailing separator if present (except for root)
          (let ((s-trimmed
                  (if (and (> (string-length s) 1)
                           (char=? (string-ref s (- (string-length s) 1)) sep))
                    (substring s 0 (- (string-length s) 1))
                    s)))
            (let loop ((i (- (string-length s-trimmed) 1)))
              (cond
                ((< i 0)
                 (if (os-windows?) (path "") (path ".")))
                ((char=? (string-ref s-trimmed i) sep)
                 (if (= i 0)
                   (path-root)
                   ;; Keep the trailing separator for the parent path
                   (path (substring s-trimmed 0 (+ i 1)))))
                (else (loop (- i 1)))
              ) ;cond
            ) ;let
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    ;;; Path predicates and operations (work with strings or paths)
    (define (path-dir? p)
      (g_isdir (path->string p))
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
          (file-not-found-error
            (string-append "No such file or directory: '" s "'")
          ) ;file-not-found-error
          (g_path-getsize s)
        ) ;if
      ) ;let
    ) ;define

    (define (path-read-text p)
      (let ((s (path->string p)))
        (if (not (file-exists? s))
          (file-not-found-error
            (string-append "No such file or directory: '" s "'")
          ) ;file-not-found-error
          (g_path-read-text s)
        ) ;if
      ) ;let
    ) ;define

    (define (path-read-bytes p)
      (let ((s (path->string p)))
        (if (not (file-exists? s))
          (file-not-found-error
            (string-append "No such file or directory: '" s "'")
          ) ;file-not-found-error
          (g_path-read-bytes s)
        ) ;if
      ) ;let
    ) ;define

    (define (path-write-text p content)
      (if (not (string? content))
        (type-error "path-write-text: content must be string")
        (g_path-write-text (path->string p) content)
      ) ;if
    ) ;define

    (define (path-append-text p content)
      (g_path-append-text (path->string p) content)
    ) ;define

    (define (path-touch p)
      (g_path-touch (path->string p))
    ) ;define

    ;;; Static path constructors
    (define (path-root)
      (make-path-record #("/") 'posix "")
    ) ;define

    (define (path-of-drive ch)
      (if (char? ch)
        (make-path-record #() 'windows (string (char-upcase ch)))
        (type-error "path-of-drive: argument must be char")
      ) ;if
    ) ;define

    (define (path-from-parts parts)
      (if (vector? parts)
        (make-path-record (vector-copy parts) 'posix "")
        (type-error "path-from-parts: argument must be vector")
      ) ;if
    ) ;define

    (define (path-from-env name)
      (path (getenv name))
    ) ;define

    (define (path-cwd)
      (path (getcwd))
    ) ;define

    (define (path-home)
      (cond
        ((or (os-linux?) (os-macos?))
         (path (getenv "HOME")))
        ((os-windows?)
         (path (string-append (getenv "HOMEDRIVE") (getenv "HOMEPATH"))))
        (else
         (value-error "path-home: unknown platform"))
      ) ;cond
    ) ;define

    (define (path-temp-dir)
      (path (os-temp-dir))
    ) ;define

    ;;; List directory contents
    (define (path-list p)
      (listdir (path->string p))
    ) ;define

    ;;; List directory contents as path objects
    (define (path-list-path p)
      (let ((base (path->string p)))
        (let ((entries (listdir base)))
          (vector-map
            (lambda (entry) (path-join base entry))
            entries
          ) ;vector-map
        ) ;let
      ) ;let
    ) ;define

    ;;; Remove directory
    (define (path-rmdir p)
      (rmdir (path->string p))
    ) ;define

    ;;; Remove file
    (define* (path-unlink p (missing-ok #f))
      (let ((s (path->string p)))
        (cond
          ((file-exists? s)
           (remove s)
          ) ;
          (missing-ok
           #t
          ) ;missing-ok
          (else
           (error 'file-not-found-error
                  (string-append "File not found: " s)
           ) ;error
          ) ;else
        ) ;cond
      ) ;let
    ) ;define*

  ) ;begin
) ;define-library
