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

;; (hint liii path) - (liii path) 的类型接口

(define-library (hint liii path)
  (import (scheme base))
  (export hint-liii/path)
  (begin

    ;; path : (string? -> path?)
    (define hint-path '((string? -> path?)))

    ;; path? : (any? -> boolean?)
    (define hint-path? '((any? -> boolean?)))

    ;; path->string : (path? -> string?)
    (define hint-path->string '((path? -> string?)))

    ;; path-join 的 segments 为可变参数，登记 2、3 个参数的形式
    (define hint-path-join
      '((string? string? -> path?) (string? string? string? -> path?))
    ) ;define

    ;; path-parent : (path? -> path?)
    (define hint-path-parent '((path? -> path?)))

    ;; path-name : (any? -> string?)
    (define hint-path-name '((any? -> string?)))

    ;; path-file? : (any? -> boolean?)
    (define hint-path-file? '((any? -> boolean?)))

    ;; path-dir? : (any? -> boolean?)
    (define hint-path-dir? '((any? -> boolean?)))

    ;; path-read-text : (any? -> string?)
    (define hint-path-read-text '((any? -> string?)))

    (define (hint-liii/path name)
      (case name
       ((path) hint-path)
       ((path?) hint-path?)
       ((path->string) hint-path->string)
       ((path-join) hint-path-join)
       ((path-parent) hint-path-parent)
       ((path-name) hint-path-name)
       ((path-file?) hint-path-file?)
       ((path-dir?) hint-path-dir?)
       ((path-read-text) hint-path-read-text)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
