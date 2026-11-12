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

;; (hint scheme eval) - (scheme eval) 的类型接口

(define-library (hint scheme eval)
  (import (scheme base))
  (export hint-scheme/eval)
  (begin

    ;; environment 接受任意个 import 集，返回求值环境说明符
    (define hint-environment '((any? -> any?)))

    ;; eval 在给定环境中求值表达式
    (define hint-eval '((any? any? -> any?)))

    (define (hint-scheme/eval name)
      (case name
       ((environment) hint-environment)
       ((eval) hint-eval)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
