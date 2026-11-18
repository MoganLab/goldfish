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
;; ; (liii uri transform) - URI 修改、路径合并与归一化子库
;; ; 函数文档见 bin/gf doc liii/uri 与 library/tests/liii/uri/transform/README.md

(define-library (liii uri transform)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii error)
    (liii uri internal parse)
    (liii uri record)
    (liii uri predicate)
  ) ;import

  ;; ; ---------- 导出接口 ----------
  ;; 修改 URI - with- 系列
  (export uri-with-scheme uri-with-host uri-with-port)
  (export uri-with-path uri-with-fragment)

  ;; 修改 URI - query 更新
  (export uri-update-query uri-extend-query)
  (export uri-without-query uri-without-query-param)

  ;; 修改 URI - 路径操作
  (export uri-join-path uri-join)
  (export uri-with-name uri-with-suffix)
  (export uri-normalize)

  (begin

    ;; ; ---------- with- 系列函数 ----------
    (define (uri-with-scheme uri-obj new-scheme)
      (make-uri-record new-scheme
        (uri-authority uri-obj)
        (uri-path uri-obj)
        (uri-query uri-obj)
        (uri-fragment uri-obj)
      ) ;make-uri-record
    ) ;define

    (define (uri-with-host uri-obj new-host)
      (let ((netloc-parts (parse-netloc (uri-authority uri-obj))))
        (make-uri-record (uri-scheme uri-obj)
          (build-netloc (list-ref netloc-parts 0)
            (list-ref netloc-parts 1)
            new-host
            (list-ref netloc-parts 3)
          ) ;build-netloc
          (uri-path uri-obj)
          (uri-query uri-obj)
          (uri-fragment uri-obj)
        ) ;make-uri-record
      ) ;let
    ) ;define

    (define (uri-with-port uri-obj new-port)
      (let ((netloc-parts (parse-netloc (uri-authority uri-obj))))
        (make-uri-record (uri-scheme uri-obj)
          (build-netloc (list-ref netloc-parts 0)
            (list-ref netloc-parts 1)
            (list-ref netloc-parts 2)
            new-port
          ) ;build-netloc
          (uri-path uri-obj)
          (uri-query uri-obj)
          (uri-fragment uri-obj)
        ) ;make-uri-record
      ) ;let
    ) ;define

    (define (uri-with-path uri-obj new-path)
      (make-uri-record (uri-scheme uri-obj)
        (uri-authority uri-obj)
        new-path
        (uri-query uri-obj)
        (uri-fragment uri-obj)
      ) ;make-uri-record
    ) ;define

    (define (uri-with-fragment uri-obj new-fragment)
      (make-uri-record (uri-scheme uri-obj)
        (uri-authority uri-obj)
        (uri-path uri-obj)
        (uri-query uri-obj)
        new-fragment
      ) ;make-uri-record
    ) ;define

    ;; ; ---------- query 更新函数 ----------
    (define (uri-update-query uri-obj updater)
      (let ((new-query (updater (uri-query uri-obj))))
        (make-uri-record (uri-scheme uri-obj)
          (uri-authority uri-obj)
          (uri-path uri-obj)
          new-query
          (uri-fragment uri-obj)
        ) ;make-uri-record
      ) ;let
    ) ;define

    (define (uri-extend-query uri-obj alist)
      (uri-update-query uri-obj (lambda (q) (append q alist)))
    ) ;define

    (define (uri-without-query uri-obj)
      (uri-update-query uri-obj (lambda (q) '()))
    ) ;define

    (define (uri-without-query-param uri-obj key)
      (uri-update-query uri-obj
        (lambda (q) (filter (lambda (p) (not (string=? (car p) key))) q))
      ) ;uri-update-query
    ) ;define

    ;; ; ---------- 路径操作与归一化 ----------
    (define (uri-normalize uri-obj)
      (if (not (uri? uri-obj))
        (type-error "uri-normalize: expected uri")
        (let* ((scheme (uri-scheme uri-obj))
               (network? (and scheme (uri-network-scheme? scheme)))
               (u
                 (if
                   (and network? (string=? (or (uri-path uri-obj) "") ""))
                   (uri-with-path uri-obj "/")
                   uri-obj
                 ) ;if
               ) ;u
               (explicit-port (uri-explicit-port u))
              ) ;
          (if (and network? explicit-port (= explicit-port (uri-default-port scheme)))
            (uri-with-port u #f)
            u
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (uri-join-path uri-obj . segments)
      (let ((current-path (uri-path uri-obj)) (new-segments (string-join segments "/")))
        (uri-with-path uri-obj
          (string-append (if (string-ends? current-path "/")
                           current-path
                           (string-append current-path "/")
                         ) ;if
            new-segments
          ) ;string-append
        ) ;uri-with-path
      ) ;let
    ) ;define

    (define (uri-with-name uri-obj new-name)
      (let* ((parent (uri-parent uri-obj))
             (path (uri-path uri-obj))
             (root? (and (string? path) (> (string-length path) 0) (char=? (string-ref path 0) #\/))
             ) ;root?
            ) ;
        (uri-with-path uri-obj
          (cond
           ((and parent (not (string=? parent ""))) (string-append parent "/" new-name))
           (root? (string-append "/" new-name))
           (else new-name)
          ) ;cond
        ) ;uri-with-path
      ) ;let*
    ) ;define

    (define (uri-with-suffix uri-obj new-suffix)
      (let ((name (uri-name uri-obj)))
        (if name
          (let ((dot-pos (string-index-right name #\.)))
            (uri-with-name uri-obj
              (if dot-pos
                (string-append (substring name 0 dot-pos) "." new-suffix)
                (string-append name "." new-suffix)
              ) ;if
            ) ;uri-with-name
          ) ;let
          uri-obj
        ) ;if
      ) ;let
    ) ;define

    (define (uri-join base-uri ref-uri)
      (let ((base-path (uri-path base-uri)) (ref-path (uri-path ref-uri)))
        (if (or (not ref-path) (string=? ref-path ""))
          base-uri
          (if (char=? (string-ref ref-path 0) #\/)
            (uri-with-path base-uri ref-path)
            (uri-with-path base-uri
              (normalize-path (string-append (uri-parent base-uri) "/" ref-path))
            ) ;uri-with-path
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    ;; ; ---------- 路径归一化辅助 ----------
    (define (normalize-path path)
      (let ((segments (string-split path "/"))
            (absolute? (and (> (string-length path) 0) (char=? (string-ref path 0) #\/)))
           ) ;
        (let loop
          ((segs segments) (result '()))
          (if (null? segs)
            (if (null? result)
              "/"
              (let ((normalized (string-join (reverse result) "/")))
                (if absolute? (string-append "/" normalized) normalized)
              ) ;let
            ) ;if
            (cond
             ((string=? (car segs) "")
              (loop (cdr segs)
                (if (and (null? (cdr segs)) (not (null? result))) (cons "" result) result)
              ) ;loop
             ) ;
             ((string=? (car segs) ".") (loop (cdr segs) result))
             ((string=? (car segs) "..")
              (loop (cdr segs) (if (null? result) result (cdr result)))
             ) ;
             (else (loop (cdr segs) (cons (car segs) result)))
            ) ;cond
          ) ;if
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
