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

;; (hint scheme write) - (scheme write) 的类型接口

(define-library (hint scheme write)
  (import (scheme base))
  (export hint-scheme/write)
  (begin

    ;; 外部表示输出，可选第二个参数为输出端口
    (define hint-display '((any? -> any?) (any? output-port? -> any?)))
    (define hint-write '((any? -> any?) (any? output-port? -> any?)))
    (define hint-write-shared '((any? -> any?) (any? output-port? -> any?)))
    (define hint-write-simple '((any? -> any?) (any? output-port? -> any?)))

    (define (hint-scheme/write name)
      (case name
       ((display) hint-display)
       ((write) hint-write)
       ((write-shared) hint-write-shared)
       ((write-simple) hint-write-simple)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
