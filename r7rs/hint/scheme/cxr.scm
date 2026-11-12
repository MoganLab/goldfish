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

;; (hint scheme cxr) - (scheme cxr) 的类型接口
;;
;; R7RS 6.4：cxr 组合子对 car/cdr 路径求值，参数为成对列表，
;; 结果为路径末端的元素（类型不定）。

(define-library (hint scheme cxr)
  (import (scheme base))
  (export hint-scheme/cxr)
  (begin

    ;; 三级组合子：caaa? .. cddd?
    (define hint-caaar '((pair? -> any?)))
    (define hint-caadr '((pair? -> any?)))
    (define hint-cadar '((pair? -> any?)))
    (define hint-caddr '((pair? -> any?)))
    (define hint-cdaar '((pair? -> any?)))
    (define hint-cdadr '((pair? -> any?)))
    (define hint-cddar '((pair? -> any?)))
    (define hint-cdddr '((pair? -> any?)))

    ;; 四级组合子：caaaar .. cddddr
    (define hint-caaaar '((pair? -> any?)))
    (define hint-caaadr '((pair? -> any?)))
    (define hint-caadar '((pair? -> any?)))
    (define hint-caaddr '((pair? -> any?)))
    (define hint-cadaar '((pair? -> any?)))
    (define hint-cadadr '((pair? -> any?)))
    (define hint-caddar '((pair? -> any?)))
    (define hint-cadddr '((pair? -> any?)))
    (define hint-cdaaar '((pair? -> any?)))
    (define hint-cdaadr '((pair? -> any?)))
    (define hint-cdadar '((pair? -> any?)))
    (define hint-cdaddr '((pair? -> any?)))
    (define hint-cddaar '((pair? -> any?)))
    (define hint-cddadr '((pair? -> any?)))
    (define hint-cdddar '((pair? -> any?)))
    (define hint-cddddr '((pair? -> any?)))

    (define (hint-scheme/cxr name)
      (case name
       ((caaar) hint-caaar)
       ((caadr) hint-caadr)
       ((cadar) hint-cadar)
       ((caddr) hint-caddr)
       ((cdaar) hint-cdaar)
       ((cdadr) hint-cdadr)
       ((cddar) hint-cddar)
       ((cdddr) hint-cdddr)
       ((caaaar) hint-caaaar)
       ((caaadr) hint-caaadr)
       ((caadar) hint-caadar)
       ((caaddr) hint-caaddr)
       ((cadaar) hint-cadaar)
       ((cadadr) hint-cadadr)
       ((caddar) hint-caddar)
       ((cadddr) hint-cadddr)
       ((cdaaar) hint-cdaaar)
       ((cdaadr) hint-cdaadr)
       ((cdadar) hint-cdadar)
       ((cdaddr) hint-cdaddr)
       ((cddaar) hint-cddaar)
       ((cddadr) hint-cddadr)
       ((cdddar) hint-cdddar)
       ((cddddr) hint-cddddr)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
