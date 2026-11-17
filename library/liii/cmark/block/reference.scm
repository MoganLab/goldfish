;;
;; Copyright (C) 2026 Liii Network
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

(define-library (liii cmark block reference)
  (import (scheme base))
  (import (scheme char))
  (import (liii cmark inline entity))
  (import (liii cmark inline escape))
  (import (liii cmark common))
  (import (liii cmark text))
  (import (liii cmark block line))
  (export make-reference-map reference-map? reference-map-add-definition!
    reference-map-resolve normalize-reference-label
    collect-link-reference-definition percent-encode-uri
  ) ;export
  (begin
    (define (make-reference-map)
      (vector 'refmap '())
    ) ;define

    (define (reference-map? obj)
      (and (vector? obj) (= (vector-length obj) 2) (eq? (vector-ref obj 0) 'refmap))
    ) ;define

    (define (reference-map-definitions refmap)
      (if (reference-map? refmap) (vector-ref refmap 1) '())
    ) ;define

    (define (reference-map-add-definition! refmap label url title)
      (let ((normalized (normalize-reference-label label)))
        (if (and (reference-map? refmap) (not (string=? normalized "")))
          (let ((existing (assoc normalized (reference-map-definitions refmap))))
            (if (not existing)
              (vector-set! refmap
                1
                (cons (cons normalized (cons url title)) (reference-map-definitions refmap))
              ) ;vector-set!
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (reference-map-resolve refmap label)
      (let ((normalized (normalize-reference-label label)))
        (if (and (reference-map? refmap) (not (string=? normalized "")))
          (assoc normalized (reference-map-definitions refmap))
          #f
        ) ;if
      ) ;let
    ) ;define

    ;; 空白折叠 + Unicode case folding（Example 206/540）
    ;; goldfish 的 string-downcase 是简单 1:1 映射，ß/ẞ 需显式折叠为 ss
    (define (unicode-case-fold str)
      (let ((len (string-length str)))
        (let loop
          ((pos 0) (parts '()))
          (cond ((>= pos len) (apply string-append (reverse parts)))
                ;; ß 是 2 字节，ẞ 是 3 字节（UTF-8）
                ((and (<= (+ pos 2) len) (string=? (substring str pos (+ pos 2)) "ß"))
                 (loop (+ pos 2) (cons "ss" parts))
                ) ;
                ((and (<= (+ pos 3) len) (string=? (substring str pos (+ pos 3)) "ẞ"))
                 (loop (+ pos 3) (cons "ss" parts))
                ) ;
                (else
                  (loop (+ pos 1) (cons (substring str pos (+ pos 1)) parts))
                ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (normalize-reference-label label)
      ;; 项目扩展：label 规范化前解码 entity（测试要求 [my&amp;ref] 匹配 my&ref）；
      ;; 不解码 backslash escape——Example 545/549/550 要求转义按字面匹配
      (let ((decoded (decode-entity-references-in-string label)))
        (string-downcase
          (let ((trimmed (string-trim (unicode-case-fold decoded))))
            (let loop
              ((chars (string->list trimmed)) (parts '()) (in-space? #f))
              (cond
               ((null? chars) (string-trim (string-join (reverse parts) "")))
               ((ascii-whitespace? (car chars))
                (if in-space?
                  (loop (cdr chars) parts #t)
                  (loop (cdr chars) (cons " " parts) #t)
                ) ;if
               ) ;
               (else
                 (loop (cdr chars) (cons (string (car chars)) parts) #f)
               ) ;else
              ) ;cond
            ) ;let
          ) ;let
        ) ;string-downcase
      ) ;let
    ) ;define

    ;; decode-backslash-escapes-in-string 复用 (liii cmark inline escape)（公共工具）

    (define (skip-ws-across pos lines crossed?)
      (let ((line (car lines)))
        (cond
         ((>= pos (string-length line))
          (if
            (and (pair? (cdr lines)) (not (blank-line? (car (cdr lines)))))
            (skip-ws-across 0 (cdr lines) #t)
            #f
          ) ;if
         ) ;
         ((ascii-whitespace? (string-ref line pos))
          (skip-ws-across (+ pos 1) lines crossed?)
         ) ;
         (else (list pos lines crossed?))
        ) ;cond
      ) ;let
    ) ;define
    (define (scan-reference-label lines start-pos)
      (let loop
        ((remaining lines) (pos start-pos) (parts '()) (escaped? #f))
        (let ((line (car remaining)))
          (cond
           ((>= pos (string-length line))
            (if (pair? (cdr remaining)) (loop (cdr remaining) 0 (cons " " parts) #f) #f)
           ) ;
           (escaped?
             (loop remaining (+ pos 1) (cons (string (string-ref line pos)) parts) #f)
           ) ;escaped?
           ;; 保留反斜杠（label 匹配用原始 normalize，Example 549/550）
           ((char=? (string-ref line pos) #\\)
            (loop remaining (+ pos 1) (cons "\\" parts) #t)
           ) ;
           ;; label 内未转义 [ 使定义非法（Example 546: [ref[]: /uri）
           ((char=? (string-ref line pos) #\[) #f)
           ((char=? (string-ref line pos) #\])
            (let* ((raw-label (string-join (reverse parts) ""))
                   (norm-label (normalize-reference-label raw-label))
                  ) ;
              (if
                (and (< (+ pos 1) (string-length line))
                  (char=? (string-ref line (+ pos 1)) #\:)
                  ;; label 至少一个非空白字符（Example 551/552）
                  (not (string=? norm-label ""))
                ) ;and
                (list raw-label (+ pos 2) remaining)
                #f
              ) ;if
            ) ;let*
           ) ;
           (else
             (loop remaining (+ pos 1) (cons (string (string-ref line pos)) parts) #f)
           ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    ;; dest 解析：`<...>` 角括号或裸解析（§3.14，按首字符区分）
    (define (parse-link-destination dest-pos)
      (if
        (char=? (string-ref (car (cadr dest-pos)) (car dest-pos)) #\<)
        (parse-angle-dest-at (car dest-pos) (cadr dest-pos) (caddr dest-pos))
        (parse-bare-dest-at (car dest-pos) (cadr dest-pos) (caddr dest-pos))
      ) ;if
    ) ;define

    ;; 完成引用定义组装（label+url+title 起止，§3.14）
    (define (finish-reference label url t-start t-end)
      (finish-reference-definition label url t-start t-end)
    ) ;define

    (define (parse-bare-dest-at pos lines crossed?)
      (let ((line (car lines)) (len (string-length (car lines))))
        (let loop
          ((i pos) (parts '()))
          (if (or (>= i len) (ascii-whitespace? (string-ref line i)))
            (let* ((raw (string-join (reverse parts) ""))
                   (unescaped (decode-entity-references-in-string raw))
                   (decoded (decode-backslash-escapes-in-string unescaped))
                  ) ;
              (cons (percent-encode-uri decoded) (list i lines crossed?))
            ) ;let*
            (loop (+ i 1) (cons (string (string-ref line i)) parts))
          ) ;if
        ) ;let
      ) ;let
    ) ;define
    (define (parse-angle-dest-at pos lines crossed?)
      (let ((line (car lines)) (len (string-length (car lines))))
        (let loop
          ((i (+ pos 1)) (parts '()))
          (cond ((>= i len) #f)
                ((char=? (string-ref line i) #\\)
                 (if (< (+ i 1) len) (loop (+ i 2) (cons (substring line i (+ i 2)) parts)) #f)
                ) ;
                ((char=? (string-ref line i) #\>)
                 (if
                   (or (= (+ i 1) len) (ascii-whitespace? (string-ref line (+ i 1))))
                   (let* ((raw (string-join (reverse parts) ""))
                          (unescaped (decode-entity-references-in-string raw))
                          (decoded (decode-backslash-escapes-in-string unescaped))
                         ) ;
                     (cons (percent-encode-uri decoded) (list (+ i 1) lines crossed?))
                   ) ;let*
                   #f
                 ) ;if
                ) ;
                (else
                  (loop (+ i 1) (cons (string (string-ref line i)) parts))
                ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (parse-reference-title quote-char pos lines)
      (let loop
        ((pos pos) (lines lines) (parts '()))
        (let ((line (car lines)))
          (cond
           ((>= pos (string-length line))
            (if
              (and (pair? (cdr lines)) (not (blank-line? (car (cdr lines)))))
              (loop 0 (cdr lines) (cons "\n" parts))
              #f
            ) ;if
           ) ;
           ((char=? (string-ref line pos) quote-char)
            (cons (decode-title-escapes (string-join (reverse parts) ""))
              (list (+ pos 1) lines #f)
            ) ;cons
           ) ;
           ((char=? (string-ref line pos) #\\)
            (if (< (+ pos 1) (string-length line))
              (loop (+ pos 2) lines (cons (substring line pos (+ pos 2)) parts))
              (loop (+ pos 1) lines (cons "\\" parts))
            ) ;if
           ) ;
           (else
             (loop (+ pos 1) lines (cons (string (string-ref line pos)) parts))
           ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (title-start-char? pos lines)
      (let ((c (string-ref (car lines) pos)))
        (or (char=? c #\") (char=? c #\') (char=? c #\())
      ) ;let
    ) ;define
    (define (parse-reference-title-part label norm-label url after-dest)
      (let ((title-pos (skip-ws-across (car after-dest) (cadr after-dest) #f)))
        (cond
         ((not title-pos) (list norm-label url "" (cdr (cadr after-dest))))
         ((title-start-char? (car title-pos) (cadr title-pos))
          (let* ((t-start (car title-pos))
                 (t-end (cadr title-pos))
                 (finished (finish-reference label url t-start t-end))
                 (no-title? (caddr title-pos))
                 (t-result (if no-title? (list norm-label url "" t-end) #f))
                ) ;
            (if finished finished t-result)
          ) ;let*
         ) ;
         ((caddr title-pos) (list norm-label url "" (cadr title-pos)))
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (parse-reference-after-label label-result)
      (let* ((label (car label-result))
             (norm-label (normalize-reference-label label))
             (pos (cadr label-result))
             (rest-lines (caddr label-result))
             (dest-pos (skip-ws-across pos rest-lines #f))
            ) ;
        (and dest-pos
          (let ((dest-result (parse-link-destination dest-pos)))
            (and dest-result
              (parse-reference-title-part label
                norm-label
                (car dest-result)
                (cdr dest-result)
              ) ;parse-reference-title-part
            ) ;and
          ) ;let
        ) ;and
      ) ;let*
    ) ;define

    (define (parse-reference-definition-lines lines)
      (let* ((first-line (car lines)) (indent (count-leading-spaces first-line)))
        (and (<= indent 3)
          (< indent (string-length first-line))
          (char=? (string-ref first-line indent) #\[)
          (let ((label-result (scan-reference-label lines (+ indent 1))))
            (and label-result (parse-reference-after-label label-result))
          ) ;let
        ) ;and
      ) ;let*
    ) ;define
    (define (finish-reference-definition label url pos lines)
      (let* ((quote-char (string-ref (car lines) pos))
             (title-result (parse-reference-title quote-char (+ pos 1) lines))
            ) ;
        (and title-result
          (let* ((title (car title-result))
                 (after-title (cdr title-result))
                 (after-pos (car after-title))
                 (after-lines (cadr after-title))
                ) ;
            (and (rest-of-line-blank? after-pos (car after-lines))
              (list (normalize-reference-label label) url title (cdr after-lines))
            ) ;and
          ) ;let*
        ) ;and
      ) ;let*
    ) ;define
    (define (rest-of-line-blank? pos line)
      (let loop
        ((i pos))
        (or (>= i (string-length line))
          (and (ascii-whitespace? (string-ref line i)) (loop (+ i 1)))
        ) ;or
      ) ;let
    ) ;define
    (define (collect-link-reference-definition lines)
      (let ((parsed (parse-reference-definition-lines lines)))
        (and parsed
          (let ((label (car parsed))
                (url (cadr parsed))
                (title (caddr parsed))
                (rest-lines (cadddr parsed))
               ) ;
            (cons (cons label (cons url title)) rest-lines)
          ) ;let
        ) ;and
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
