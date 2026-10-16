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

(define-library (liii tempfile)
  (export mkstemp mkdtemp with-temp-file with-temp-dir)
  (import (scheme base) (scheme file) (liii base) (liii error) (liii os))
  (begin

    (define (%validate-dir caller dir)
      (when dir
        (cond ((not (file-exists? dir))
               (file-not-found-error (string-append caller ": directory not found: " dir))
              ) ;
              ((not (access dir 'W_OK))
               (permission-denied-error (string-append caller ": permission denied: " dir))
              ) ;
        ) ;cond
      ) ;when
    ) ;define

    (define* (mkstemp (prefix "tmp") (suffix "") (dir #f))
      (unless (string? prefix)
        (type-error "mkstemp: prefix must be a string")
      ) ;unless
      (unless (string? suffix)
        (type-error "mkstemp: suffix must be a string")
      ) ;unless
      (unless (or (not dir) (string? dir))
        (type-error "mkstemp: dir must be a string or #f")
      ) ;unless
      (%validate-dir "mkstemp" dir)
      (let ((target-dir (if dir dir (os-temp-dir))))
        (g_mkstemp prefix suffix target-dir)
      ) ;let
    ) ;define*

    (define* (mkdtemp (prefix "tmp") (suffix "") (dir #f))
      (unless (string? prefix)
        (type-error "mkdtemp: prefix must be a string")
      ) ;unless
      (unless (string? suffix)
        (type-error "mkdtemp: suffix must be a string")
      ) ;unless
      (unless (or (not dir) (string? dir))
        (type-error "mkdtemp: dir must be a string or #f")
      ) ;unless
      (%validate-dir "mkdtemp" dir)
      (let ((target-dir (if dir dir (os-temp-dir))))
        (g_mkdtemp prefix suffix target-dir)
      ) ;let
    ) ;define*

    (define* (with-temp-file proc (prefix "tmp") (suffix "") (dir #f))
      (unless (procedure? proc)
        (type-error "with-temp-file: proc must be a procedure")
      ) ;unless
      (unless (string? prefix)
        (type-error "with-temp-file: prefix must be a string")
      ) ;unless
      (unless (string? suffix)
        (type-error "with-temp-file: suffix must be a string")
      ) ;unless
      (unless (or (not dir) (string? dir))
        (type-error "with-temp-file: dir must be a string or #f")
      ) ;unless
      (let*-values (((port path) (mkstemp prefix suffix dir)))
        (close-output-port port)
        (dynamic-wind (lambda () #f)
          (lambda () (proc path))
          (lambda () (when (file-exists? path) (delete-file path)))
        ) ;dynamic-wind
      ) ;let*-values
    ) ;define*

    (define* (with-temp-dir proc (prefix "tmp") (suffix "") (dir #f))
      (unless (procedure? proc)
        (type-error "with-temp-dir: proc must be a procedure")
      ) ;unless
      (unless (string? prefix)
        (type-error "with-temp-dir: prefix must be a string")
      ) ;unless
      (unless (string? suffix)
        (type-error "with-temp-dir: suffix must be a string")
      ) ;unless
      (unless (or (not dir) (string? dir))
        (type-error "with-temp-dir: dir must be a string or #f")
      ) ;unless
      (let ((path (mkdtemp prefix suffix dir)))
        (dynamic-wind (lambda () #f)
          (lambda () (proc path))
          (lambda () (when (file-exists? path) (g_rmtree path)))
        ) ;dynamic-wind
      ) ;let
    ) ;define*

  ) ;begin
) ;define-library
