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

;; (hint scheme load) - (scheme load) 的类型接口

(define-library (hint scheme load)
  (import (scheme base))
  (export hint-scheme/load)
  (begin

    ;; load 读取文件并求值，可选第二个参数为求值环境
    (define hint-load '((string? -> any?) (string? any? -> any?)))

    (define (hint-scheme/load name)
      (case name
       ((load) hint-load)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
