;;; liii/bigint/predicate.scm - bigint 谓词函数
;;;
;;; 负责 bigint 的各种谓词判断。
;;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;  http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(define-library (liii bigint predicate)
  (import (scheme base) (liii error) (liii bigint record))

  (export bigint? bigint-zero? bigint-positive? bigint-negative? bigint-even?
    bigint-odd?
  ) ;export

  (begin
    (define (ensure-bigint who obj)
      (if (bigint? obj)
        obj
        (type-error (string-append who " in (liii bigint predicate): not a bigint") obj)
      ) ;if
    ) ;define

    ;;; ---------- 基本谓词 ----------
    ;; bigint? 已由 bigint-record 提供，这里通过重新导出使接口完整

    (define (bigint-zero? n)
      (= (bigint-signum (ensure-bigint "bigint-zero?" n)) 0)
    ) ;define

    (define (bigint-positive? n)
      (= (bigint-signum (ensure-bigint "bigint-positive?" n)) 1)
    ) ;define

    (define (bigint-negative? n)
      (= (bigint-signum (ensure-bigint "bigint-negative?" n)) -1)
    ) ;define

    ;;; ---------- 奇偶判断 ----------
    (define (bigint-even? n)
      (let ((value (ensure-bigint "bigint-even?" n)))
        (let ((mag (bigint-mag value)))
          (or (null? mag) (let ((lsb (car (reverse mag)))) (even? lsb)))
        ) ;let
      ) ;let
    ) ;define

    (define (bigint-odd? n)
      (not (bigint-even? n))
    ) ;define

  ) ;begin
) ;define-library
