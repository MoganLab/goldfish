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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

;;
;; Based on Alex Shinn's SRFI-166 implementation:
;; Copyright (c) 2006-2020 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;

(define-library (liii format color)
  (import (scheme base)
    (scheme case-lambda)
    (liii error)
    (liii string)
    (rename (srfi srfi-166 color)
      (as-true-color %as-true-color/rgb)
      (on-true-color %on-true-color/rgb)
    ) ;rename
  ) ;import
  (export
    ;; foreground
    as-red
    as-blue
    as-green
    as-cyan
    as-yellow
    as-magenta
    as-white
    as-black
    as-bold
    as-italic
    as-underline
    as-color
    as-true-color
    ;; background
    on-red
    on-blue
    on-green
    on-cyan
    on-yellow
    on-magenta
    on-white
    on-black
    on-color
    on-true-color
  ) ;export
  (begin

    ;; 0~255 的精确整数，即合法的 RGB 分量
    (define (%rgb-component? x)
      (and (exact-integer? x) (<= 0 x 255))
    ) ;define

    ;; '(r g b)：恰为 3 个 0~255 精确整数的列表
    (define (%rgb-list? x)
      (and (pair? x)
        (pair? (cdr x))
        (pair? (cddr x))
        (null? (cdddr x))
        (%rgb-component? (car x))
        (%rgb-component? (cadr x))
        (%rgb-component? (caddr x))
      ) ;and
    ) ;define

    ;; hex 颜色字符串 -> '(r g b)，接受 "#8abeb7" 或 "8abeb7"，大小写均可
    (define (%hex->rgb s)
      (let* ((body (if (string-starts? s "#") (substring s 1 (string-length s)) s))
             (n (if (= 6 (string-length body)) (string->number (string-append "#x" body)) #f)
             ) ;n
            ) ;
        (if (and n (exact-integer? n))
          (list (quotient n 65536) (remainder (quotient n 256) 256) (remainder n 256))
          (type-error "invalid hex color string" s)
        ) ;if
      ) ;let*
    ) ;define

    ;; 颜色值（RGB 列表或 hex 字符串）-> '(r g b)
    (define (%color->rgb24 x)
      (cond ((%rgb-list? x) x)
            ((string? x) (%hex->rgb x))
            (else (type-error "invalid color" x))
      ) ;cond
    ) ;define

    ;; 散参约定分发：r g b 均为合法分量时为 srfi-166 散参形式；
    ;; r 为颜色值（RGB 列表或 hex 字符串）时，被散参子句绑定的 g、b 归入内容
    (define (%true-color-args r g b fmt)
      (cond ((and (%rgb-component? r) (%rgb-component? g) (%rgb-component? b))
             (append (list r g b) fmt)
            ) ;
            ((or (pair? r) (string? r)) (append (%color->rgb24 r) (list g b) fmt))
            (else (type-error "invalid true-color arguments" r))
      ) ;cond
    ) ;define

    ;; 单值约定分发：color 必须是 RGB 列表或 hex 字符串，fmt 均为内容
    (define (%true-color-args/color color fmt)
      (append (%color->rgb24 color) fmt)
    ) ;define

    ;; 增强版：颜色可以是 RGB 列表、hex 字符串，也兼容 srfi-166 散参形式。
    ;; case-lambda 按参数个数分发：3 个以上匹配散参形式，其余匹配单值形式
    (define as-true-color
      (case-lambda
       ((r g b . fmt) (apply %as-true-color/rgb (%true-color-args r g b fmt)))
       ((color . fmt) (apply %as-true-color/rgb (%true-color-args/color color fmt)))
      ) ;case-lambda
    ) ;define

    (define on-true-color
      (case-lambda
       ((r g b . fmt) (apply %on-true-color/rgb (%true-color-args r g b fmt)))
       ((color . fmt) (apply %on-true-color/rgb (%true-color-args/color color fmt)))
      ) ;case-lambda
    ) ;define

  ) ;begin
) ;define-library
