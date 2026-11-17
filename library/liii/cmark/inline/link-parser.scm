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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii cmark inline link-parser)
  (import (scheme base))
  (import (liii cmark inline link))
  (import (liii cmark block reference))
  (import (liii cmark node))
  (export link-start? image-start? contains-link-node? resolve-reference-target
    parse-link-with parse-image-with
  ) ;export
  (begin

    (define (link-start? str pos)
      (and (< pos (string-length str)) (char=? (string-ref str pos) #\[))
    ) ;define

    (define (image-start? str pos)
      (and (< (+ pos 1) (string-length str))
        (char=? (string-ref str pos) #\!)
        (char=? (string-ref str (+ pos 1)) #\[)
      ) ;and
    ) ;define

    (define (resolve-reference-target refmap label)
      (let ((entry (reference-map-resolve refmap label)))
        (and entry (cdr entry))
      ) ;let
    ) ;define

    (define (make-link-node-from-reference text-content reference-target refmap
              parse-inlines-fn
            ) ;make-link-node-from-reference
      (let ((link-node (make-cmark-link (car reference-target) (cdr reference-target))))
        (for-each (lambda (node) (cmark-node-append-child! link-node node))
          (parse-inlines-fn text-content refmap)
        ) ;for-each
        link-node
      ) ;let
    ) ;define

    (define (make-image-node-from-reference alt-text reference-target refmap
              parse-inlines-fn
            ) ;make-image-node-from-reference
      (let ((image-node (make-cmark-image (car reference-target) (cdr reference-target)))
            (alt-nodes (parse-inlines-fn alt-text refmap))
           ) ;
        (for-each (lambda (node) (cmark-node-append-child! image-node node))
          (if (null? alt-nodes) (list (make-cmark-text "")) alt-nodes)
        ) ;for-each
        image-node
      ) ;let
    ) ;define

    (define (link-node-cons text target refmap end parse-inlines-fn)
      (cons (make-link-node-from-reference text target refmap parse-inlines-fn)
        (+ end 1)
      ) ;cons
    ) ;define

    (define (image-node-cons alt-text target refmap end parse-inlines-fn)
      (cons (make-image-node-from-reference alt-text target refmap parse-inlines-fn)
        (+ end 1)
      ) ;cons
    ) ;define

    (define (contains-link-node? nodes)
      (if (null? nodes)
        #f
        (or (eq? (cmark-node-type (car nodes)) 'link)
          (contains-link-node? (cmark-node-children (car nodes)))
          (contains-link-node? (cdr nodes))
        ) ;or
      ) ;if
    ) ;define

    (define (make-link-node-with-nodes url title text-nodes)
      (let ((link-node (make-cmark-link url title)))
        (for-each (lambda (node) (cmark-node-append-child! link-node node)) text-nodes)
        link-node
      ) ;let
    ) ;define

    (define (parse-inline-link str start bracket-end text-content text-nodes
              refmap parse-inlines-fn
            ) ;parse-inline-link
      (let ((result (parse-inline-link-rest str (+ bracket-end 2))))
        (if result
          (let* ((link-target (car result))
                 (link-node (make-link-node-with-nodes (car link-target) (cdr link-target) text-nodes)
                 ) ;link-node
                ) ;
            (cons link-node (cdr result))
          ) ;let*
          (let ((target (resolve-reference-target refmap text-content)))
            (if target
              (link-node-cons text-content target refmap bracket-end parse-inlines-fn)
              (cons (make-cmark-text "[") (+ start 1))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (parse-ref-link str start len bracket-end text-content refmap parse-inlines-fn)
      (let ((ref-start (+ bracket-end 2)))
        (if (and (< ref-start len) (char=? (string-ref str ref-start) #\]))
          (let ((target (resolve-reference-target refmap text-content)))
            (if target
              (cons (make-link-node-from-reference text-content target refmap parse-inlines-fn)
                (+ ref-start 1)
              ) ;cons
              (cons (make-cmark-text "[") (+ start 1))
            ) ;if
          ) ;let
          (let ((ref-end (find-matching-bracket str (+ bracket-end 1))))
            (if ref-end
              (let* ((label (substring str (+ bracket-end 2) ref-end))
                     (target (resolve-reference-target refmap label))
                    ) ;
                (if target
                  (link-node-cons text-content target refmap ref-end parse-inlines-fn)
                  (cons (make-cmark-text "[") (+ start 1))
                ) ;if
              ) ;let*
              (cons (make-cmark-text "[") (+ start 1))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (parse-link-with str start refmap parse-inlines-fn)
      (let* ((len (string-length str)) (bracket-end (find-matching-bracket str start)))
        (if (not bracket-end)
          (cons (make-cmark-text "[") (+ start 1))
          (let* ((text-content (substring str (+ start 1) bracket-end))
                 (text-nodes (parse-inlines-fn text-content refmap))
                ) ;
            (cond ((contains-link-node? text-nodes) (cons (make-cmark-text "[") (+ start 1)))
                  ((and (< (+ bracket-end 1) len) (char=? (string-ref str (+ bracket-end 1)) #\())
                   (parse-inline-link str start bracket-end text-content
                     text-nodes refmap parse-inlines-fn
                   ) ;parse-inline-link
                  ) ;
                  ((and (< (+ bracket-end 1) len) (char=? (string-ref str (+ bracket-end 1)) #\[))
                   (parse-ref-link str start len bracket-end text-content refmap parse-inlines-fn)
                  ) ;
                  (else
                    (let ((target (resolve-reference-target refmap text-content)))
                      (if target
                        (cons (make-link-node-from-reference text-content target refmap parse-inlines-fn)
                          (+ bracket-end 1)
                        ) ;cons
                        (cons (make-cmark-text "[") (+ start 1))
                      ) ;if
                    ) ;let
                  ) ;else
            ) ;cond
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (parse-inline-image str start bracket-end alt-text refmap parse-inlines-fn)
      (let ((result (parse-inline-link-rest str (+ bracket-end 2))))
        (if result
          (let* ((url (car (car result)))
                 (title (cdr (car result)))
                 (image-node (make-cmark-image url title))
                 (alt-nodes (parse-inlines-fn alt-text refmap))
                ) ;
            (for-each (lambda (node) (cmark-node-append-child! image-node node))
              (if (null? alt-nodes) (list (make-cmark-text "")) alt-nodes)
            ) ;for-each
            (cons image-node (cdr result))
          ) ;let*
          (cons (make-cmark-text "!") (+ start 1))
        ) ;if
      ) ;let
    ) ;define

    (define (parse-ref-image str start len bracket-end alt-text refmap parse-inlines-fn)
      (let ((ref-start (+ bracket-end 2)))
        (if (and (< ref-start len) (char=? (string-ref str ref-start) #\]))
          (let ((target (resolve-reference-target refmap alt-text)))
            (if target
              (image-node-cons alt-text target refmap ref-start parse-inlines-fn)
              (cons (make-cmark-text "!") (+ start 1))
            ) ;if
          ) ;let
          (let ((ref-end (find-matching-bracket str (+ bracket-end 1))))
            (if ref-end
              (let* ((label (substring str (+ bracket-end 2) ref-end))
                     (target (resolve-reference-target refmap label))
                    ) ;
                (if target
                  (image-node-cons alt-text target refmap ref-end parse-inlines-fn)
                  (cons (make-cmark-text "!") (+ start 1))
                ) ;if
              ) ;let*
              (cons (make-cmark-text "!") (+ start 1))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (parse-image-with str start refmap parse-inlines-fn)
      (let* ((len (string-length str))
             (bracket-end (find-matching-bracket str (+ start 1)))
            ) ;
        (if (not bracket-end)
          (cons (make-cmark-text "!") (+ start 1))
          (let ((alt-text (substring str (+ start 2) bracket-end)))
            (cond
             ((and (< (+ bracket-end 1) len) (char=? (string-ref str (+ bracket-end 1)) #\())
              (parse-inline-image str start bracket-end alt-text refmap parse-inlines-fn)
             ) ;
             ((and (< (+ bracket-end 1) len) (char=? (string-ref str (+ bracket-end 1)) #\[))
              (parse-ref-image str start len bracket-end alt-text refmap parse-inlines-fn)
             ) ;
             (else
               (let ((target (resolve-reference-target refmap alt-text)))
                 (if target
                   (image-node-cons alt-text target refmap bracket-end parse-inlines-fn)
                   (cons (make-cmark-text (substring str start (+ bracket-end 1)))
                     (+ bracket-end 1)
                   ) ;cons
                 ) ;if
               ) ;let
             ) ;else
            ) ;cond
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
