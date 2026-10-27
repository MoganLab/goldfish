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

;; (hint liii goldcheck path) - (liii goldcheck path) 的类型接口

(define-library (hint liii goldcheck path)
  (import (scheme base))
  (export hint-liii/goldcheck/path)
  (begin

    ;; normalize-sep : (string? -> string?)
    (define hint-normalize-sep '((string? -> string?)))

    ;; has-wildcard? : (string? -> boolean?)
    (define hint-has-wildcard? '((string? -> boolean?)))

    ;; string-index-of : (string? string? integer? -> (or #f integer?))
    (define hint-string-index-of '((string? string?
                                     integer?
                                     ->
                                     (or #f integer?))))

    ;; string-starts-with? : (string? string? -> boolean?)
    (define hint-string-starts-with? '((string? string? -> boolean?)))

    ;; glob-match? : (string? string? -> boolean?)
    (define hint-glob-match? '((string? string? -> boolean?)))

    ;; path-matches-exclude? : (string? string? -> boolean?)
    (define hint-path-matches-exclude? '((string? string? -> boolean?)))

    ;; exclude-entry->path : (any? -> string?)
    (define hint-exclude-entry->path '((any? -> string?)))

    ;; file-excluded? : (string? list? -> boolean?)
    (define hint-file-excluded? '((string? list? -> boolean?)))

    ;; relativize-path : (string? -> string?)
    (define hint-relativize-path '((string? -> string?)))

    (define (hint-liii/goldcheck/path name)
      (case name
       ((normalize-sep) hint-normalize-sep)
       ((has-wildcard?) hint-has-wildcard?)
       ((string-index-of) hint-string-index-of)
       ((string-starts-with?) hint-string-starts-with?)
       ((glob-match?) hint-glob-match?)
       ((path-matches-exclude?) hint-path-matches-exclude?)
       ((exclude-entry->path) hint-exclude-entry->path)
       ((file-excluded?) hint-file-excluded?)
       ((relativize-path) hint-relativize-path)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
