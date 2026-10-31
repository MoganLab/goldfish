;;
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
;;

(define-library (liii json schema format-uri)
  (import (scheme base)
    (scheme char)
    (liii json schema format-helpers)
    (liii json schema regex)
    (liii string)
  ) ;import
  (export uri? uri-reference? json-pointer? relative-json-pointer? regex?)
  (begin
    ;; ---- RFC 3986 character classes ---------------------------------------

    (define (unreserved? char)
      (or (json-schema-ascii-alnum? char)
        (char=? char #\-)
        (char=? char #\.)
        (char=? char #\_)
        (char=? char #\~)
      ) ;or
    ) ;define
    (define (sub-delim? char)
      (or (char=? char #\!)
        (char=? char #\$)
        (char=? char #\&)
        (char=? char #\')
        (char=? char #\()
        (char=? char #\))
        (char=? char #\*)
        (char=? char #\+)
        (char=? char #\,)
        (char=? char #\;)
        (char=? char #\=)
      ) ;or
    ) ;define
    (define (pchar? char)
      (or (unreserved? char) (sub-delim? char) (char=? char #\:) (char=? char #\@))
    ) ;define
    (define (qchar? char)
      (or (pchar? char) (char=? char #\/) (char=? char #\?))
    ) ;define
    (define (userinfo-char? char)
      (or (unreserved? char) (sub-delim? char) (char=? char #\:))
    ) ;define
    (define (reg-name-char? char)
      (or (unreserved? char) (sub-delim? char))
    ) ;define
    ;; Characters allowed inside an IP literal: an IPv6 address uses hex
    ;; digits and ":", IPvFuture adds "v" and letters.
    (define (ip-literal-char? char)
      (or (json-schema-hex-digit? char)
        (char=? char #\:)
        (char=? char #\.)
        (char=? char #\v)
        (char=? char #\V)
      ) ;or
    ) ;define
    (define (hex-value char)
      (cond ((and (char>=? char #\0) (char<=? char #\9)) (- (char->integer char) 48))
            ((and (char>=? char #\a) (char<=? char #\f)) (+ 10 (- (char->integer char) 97)))
            ((and (char>=? char #\A) (char<=? char #\F)) (+ 10 (- (char->integer char) 65)))
            (else #f)
      ) ;cond
    ) ;define

    ;; ---- scanning ---------------------------------------------------------

    (define (index-of-char value char)
      (index-of-char-from value char 0)
    ) ;define
    (define (index-of-char-from value char start)
      (let loop
        ((index start))
        (cond ((>= index (string-length value)) #f)
              ((char=? (string-ref value index) char) index)
              (else (loop (+ index 1)))
        ) ;cond
      ) ;let
    ) ;define
    (define (last-char-index value char)
      (let loop
        ((index (- (string-length value) 1)) (found #f))
        (cond ((< index 0) found)
              ((char=? (string-ref value index) char) (loop (- index 1) index))
              (else (loop (- index 1) found))
        ) ;cond
      ) ;let
    ) ;define
    ;; Returns the index after a valid percent escape or #f.
    (define (scan-percent value index limit)
      (if
        (and (< (+ index 2) limit)
          (hex-value (string-ref value (+ index 1)))
          (hex-value (string-ref value (+ index 2)))
        ) ;and
        (+ index 3)
        #f
      ) ;if
    ) ;define
    ;; Scans characters allowed by the predicate. Stops at the first
    ;; character that is not allowed and returns its index; returns #f
    ;; only for a malformed percent escape.
    (define (scan-chars value index limit allowed?)
      (let loop
        ((index index))
        (cond ((>= index limit) index)
              ((char=? (string-ref value index) #\%)
               (let ((next (scan-percent value index limit)))
                 (if next (loop next) #f)
               ) ;let
              ) ;
              ((allowed? (string-ref value index)) (loop (+ index 1)))
              (else index)
        ) ;cond
      ) ;let
    ) ;define
    ;; True when the whole range consists of allowed characters.
    (define (scan-all? value index limit allowed?)
      (let ((next (scan-chars value index limit allowed?)))
        (and next (= next limit))
      ) ;let
    ) ;define
    (define (scan-pchar value index limit)
      (scan-chars value index limit pchar?)
    ) ;define
    (define (scan-qchar value index limit)
      (scan-chars value index limit qchar?)
    ) ;define

    ;; ---- URI ---------------------------------------------------------------

    (define (split-once value char)
      (let ((index (index-of-char value char)))
        (if index
          (cons (substring value 0 index)
            (substring value (+ index 1) (string-length value))
          ) ;cons
          #f
        ) ;if
      ) ;let
    ) ;define
    ;; A scheme is ALPHA *( ALPHA / DIGIT / "+" / "-" / "." ).
    (define (scheme? value)
      (and (> (string-length value) 0)
        (json-schema-ascii-letter? (string-ref value 0))
        (let loop
          ((index 1))
          (cond ((>= index (string-length value)) #t)
                ((or (json-schema-ascii-alnum? (string-ref value index))
                   (char=? (string-ref value index) #\+)
                   (char=? (string-ref value index) #\-)
                   (char=? (string-ref value index) #\.)
                 ) ;or
                 (loop (+ index 1))
                ) ;
                (else #f)
          ) ;cond
        ) ;let
      ) ;and
    ) ;define
    ;; The scheme separator must not be preceded by a path separator.
    (define (scheme-index value)
      (let loop
        ((index 0))
        (cond ((>= index (string-length value)) #f)
              ((char=? (string-ref value index) #\/) #f)
              ((char=? (string-ref value index) #\:) index)
              (else (loop (+ index 1)))
        ) ;cond
      ) ;let
    ) ;define
    (define (host? value)
      (if (and (> (string-length value) 0) (char=? (string-ref value 0) #\[))
        (and (> (string-length value) 1)
          (char=? (string-ref value (- (string-length value) 1)) #\])
          (scan-all? value 1 (- (string-length value) 1) ip-literal-char?)
        ) ;and
        (scan-all? value 0 (string-length value) reg-name-char?)
      ) ;if
    ) ;define
    (define (port? value)
      (let loop
        ((index 0))
        (cond ((>= index (string-length value)) #t)
              ((json-schema-ascii-digit? (string-ref value index)) (loop (+ index 1)))
              (else #f)
        ) ;cond
      ) ;let
    ) ;define
    ;; The authority is [ userinfo "@" ] host [ ":" port ].
    (define (authority-colon host-port)
      (let ((bracket
              (if (and (> (string-length host-port) 0) (char=? (string-ref host-port 0) #\[))
                (index-of-char-from host-port #\] 1)
                #f
              ) ;if
            ) ;bracket
           ) ;
        (if bracket
          (index-of-char-from host-port #\: (+ bracket 1))
          (last-char-index host-port #\:)
        ) ;if
      ) ;let
    ) ;define
    (define (authority? value)
      (let ((at (last-char-index value #\@)))
        (let ((userinfo (if at (substring value 0 at) ""))
              (host-port (if at (substring value (+ at 1) (string-length value)) value))
             ) ;
          (let ((colon (authority-colon host-port)))
            (let ((host (if colon (substring host-port 0 colon) host-port))
                  (port (if colon (substring host-port (+ colon 1) (string-length host-port)) ""))
                 ) ;
              (and (scan-all? userinfo 0 (string-length userinfo) userinfo-char?)
                (host? host)
                (port? port)
              ) ;and
            ) ;let
          ) ;let
        ) ;let
      ) ;let
    ) ;define
    (define (path? value)
      (let loop
        ((index 0))
        (cond ((>= index (string-length value)) #t)
              ((char=? (string-ref value index) #\/)
               (let ((next (scan-pchar value (+ index 1) (string-length value))))
                 (if next (loop next) #f)
               ) ;let
              ) ;
              (else
                (let ((next (scan-pchar value index (string-length value))))
                  (if (and next (> next index)) (loop next) #f)
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define
    (define (hier-part? value)
      (if (and (>= (string-length value) 2)
            (char=? (string-ref value 0) #\/)
            (char=? (string-ref value 1) #\/)
          ) ;and
        (let ((slash (index-of-char-from value #\/ 2)))
          (if slash
            (and (authority? (substring value 2 slash))
              (path? (substring value slash (string-length value)))
            ) ;and
            (authority? (substring value 2 (string-length value)))
          ) ;if
        ) ;let
        (path? value)
      ) ;if
    ) ;define
    ;; Validates the part before the query and the fragment.
    (define (uri-part? value)
      (let ((colon (scheme-index value)))
        (if colon
          (and (scheme? (substring value 0 colon))
            (hier-part? (substring value (+ colon 1) (string-length value)))
          ) ;and
          (hier-part? value)
        ) ;if
      ) ;let
    ) ;define
    ;; A URI reference is a URI or a relative reference.
    (define (uri-reference? value)
      (let* ((length (string-length value))
             (hash (index-of-char value #\#))
             (before-hash (if hash (substring value 0 hash) value))
             (after-hash (if hash (substring value (+ hash 1) length) #f))
             (question (index-of-char before-hash #\?))
             (before-query (if question (substring before-hash 0 question) before-hash))
             (after-query (if question
                            (substring before-hash (+ question 1) (string-length before-hash))
                            #f
                          ) ;if
             ) ;after-query
            ) ;
        (and (or (not hash) (scan-all? after-hash 0 (string-length after-hash) qchar?))
          (or (not question) (scan-all? after-query 0 (string-length after-query) qchar?))
          (uri-part? before-query)
        ) ;and
      ) ;let*
    ) ;define
    ;; A URI must have a scheme.
    (define (uri? value)
      (and (uri-reference? value)
        (let ((colon (scheme-index value)))
          (and colon (scheme? (substring value 0 colon)))
        ) ;let
      ) ;and
    ) ;define

    ;; ---- JSON Pointer ------------------------------------------------------

    ;; A JSON pointer is "" or a sequence of "/" reference tokens.
    (define (json-pointer? value)
      (or (string=? value "")
        (and (char=? (string-ref value 0) #\/)
          (let loop
            ((index 0))
            (cond ((>= index (string-length value)) #t)
                  ((char=? (string-ref value index) #\/)
                   (let ((next (scan-token value (+ index 1))))
                     (if next (loop next) #f)
                   ) ;let
                  ) ;
                  (else #f)
            ) ;cond
          ) ;let
        ) ;and
      ) ;or
    ) ;define
    ;; A reference token allows "~" only as "~0" or "~1".
    (define (scan-token value index)
      (let loop
        ((index index))
        (cond ((>= index (string-length value)) index)
              ((char=? (string-ref value index) #\~)
               (if
                 (and (< (+ index 1) (string-length value))
                   (or (char=? (string-ref value (+ index 1)) #\0)
                     (char=? (string-ref value (+ index 1)) #\1)
                   ) ;or
                 ) ;and
                 (loop (+ index 2))
                 #f
               ) ;if
              ) ;
              ((char=? (string-ref value index) #\/) index)
              (else (loop (+ index 1)))
        ) ;cond
      ) ;let
    ) ;define
    ;; A relative JSON pointer is a non-negative index, optional "#" and a pointer.
    (define (relative-json-pointer? value)
      (let* ((length (string-length value)) (digits (digit-count value)))
        (and (> digits 0)
          (not (and (> digits 1) (char=? (string-ref value 0) #\0)))
          (let ((rest (substring value digits length)))
            (if (string=? rest "")
              #t
              (if (char=? (string-ref rest 0) #\#)
                (let ((pointer (substring rest 1 (string-length rest))))
                  (or (string=? pointer "") (json-pointer? pointer))
                ) ;let
                (json-pointer? rest)
              ) ;if
            ) ;if
          ) ;let
        ) ;and
      ) ;let*
    ) ;define
    (define (digit-count value)
      (let loop
        ((index 0) (count 0))
        (if (and (< index (string-length value))
              (json-schema-ascii-digit? (string-ref value index))
            ) ;and
          (loop (+ index 1) (+ count 1))
          count
        ) ;if
      ) ;let
    ) ;define

    ;; ---- regex ------------------------------------------------------------

    (define (regex? value)
      (regex-valid? value)
    ) ;define
  ) ;begin
) ;define-library
