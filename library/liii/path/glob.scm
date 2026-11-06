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
;;

(define-library (liii path glob)
  (export path-match path-glob path-rglob)
  (import (scheme base)
    (scheme char)
    (liii base)
    (liii error)
    (liii string)
    (liii vector)
    (liii path record)
    (liii path pure)
  ) ;import
  (begin

    ;; ; 字符类 [..] 匹配单字符 ch。pattern[j] 是 '['。
    ;; ; 返回 (values matched? next-index-after-])。
    (define (charclass-match-one pattern plen j ch)
      (let ((negate (and (< (+ j 1) plen) (char=? (string-ref pattern (+ j 1)) #\^))))
        (let scan
          ((k (if negate (+ j 2) (+ j 1))) (matched #f))
          (cond
           ((or (>= k plen) (char=? (string-ref pattern k) #\]))
            (values (if negate (not matched) matched) (if (< k plen) (+ k 1) k))
           ) ;
           ((and (< (+ k 2) plen) (char=? (string-ref pattern (+ k 1)) #\-))
            (let ((lo (string-ref pattern k)) (hi (string-ref pattern (+ k 2))))
              (let ((hit (if (char<=? lo hi)
                           (and (char>=? ch lo) (char<=? ch hi))
                           (and (char>=? ch hi) (char<=? ch lo))
                         ) ;if
                    ) ;hit
                   ) ;
                (scan (+ k 3) (or matched hit))
              ) ;let
            ) ;let
           ) ;
           (else
             (scan (+ k 1) (or matched (char=? (string-ref pattern k) ch)))
           ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    ;; ; glob 单层匹配(支持 * / ? / [seq],不跨分隔符)。平台无关
    ;; ; (大小写敏感性由 path-match 在 Windows 类型下统一 downcase 处理)。
    (define (glob-match? pattern str)
      (let ((plen (string-length pattern)) (slen (string-length str)))
        (letrec ((match-at
                   (lambda (p0 s0)
                     (cond ((= p0 plen) (= s0 slen))
                           ((char=? (string-ref pattern p0) #\*)
                            ;; * 尝试匹配 0..(slen-s0) 个字符
                            (let try-star
                              ((n 0))
                              (cond ((match-at (+ p0 1) (+ s0 n)) #t)
                                    ((< (+ s0 n) slen) (try-star (+ n 1)))
                                    (else #f)
                              ) ;cond
                            ) ;let
                           ) ;
                           ((= s0 slen) #f)
                           ((char=? (string-ref pattern p0) #\?) (match-at (+ p0 1) (+ s0 1)))
                           ((char=? (string-ref pattern p0) #\[)
                            (let-values (((hit next) (charclass-match-one pattern plen p0 (string-ref str s0))))
                              (and hit (match-at next (+ s0 1)))
                            ) ;let-values
                           ) ;
                           ((char=? (string-ref pattern p0) (string-ref str s0))
                            (match-at (+ p0 1) (+ s0 1))
                           ) ;
                           (else #f)
                     ) ;cond
                   ) ;lambda
                 ) ;match-at
                ) ;
          (match-at 0 0)
        ) ;letrec
      ) ;let
    ) ;define

    ;; ; 对齐 pathlib.PurePath.match(pattern)
    ;; ; 差异:Windows 类型大小写不敏感(Foo.TXT 匹配 *.txt),posix 类型大小写敏感。
    ;; ; 模式可含分隔符,从右向左逐段匹配;模式以分隔符开头(绝对模式)要求段数完全匹配。
    (define (path-match p pattern)
      (let* ((pp (path p))
             (type (path-record-type pp))
             (sep (if (eq? type 'windows) #\\ #\/))
             ;; Windows match 大小写不敏感:统一转小写后再匹配。
             (ci? (eq? type 'windows))
             (down (lambda (s) (if ci? (string-downcase s) s)))
             (segs (map down (vector->list (path-record-parts pp))))
             (pat-raw (map down (string-split pattern sep)))
             ;; 绝对模式:pattern 以 sep 开头会产生首段空串
             (absolute-pattern? (and (>= (string-length pattern) 1) (char=? (string-ref pattern 0) sep))
             ) ;absolute-pattern?
             (pat-segs (if absolute-pattern? (cdr pat-raw) pat-raw))
            ) ;
        ;; 逐段从右匹配 pat-segs 与 segs 的尾部;绝对模式要求段数完全匹配
        (let loop
          ((ps (reverse pat-segs)) (ss (reverse segs)))
          (cond ((null? ps) (or (not absolute-pattern?) (null? ss)))
                ((null? ss) #f)
                ((glob-match? (car ps) (car ss)) (loop (cdr ps) (cdr ss)))
                (else #f)
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    ;; ; 目录存在性/类型由 C 层单次 stat 判定，此处仅做类型校验
    (define (%glob-entries who p pattern recursive)
      (unless (or (string? p) (path? p))
        (type-error (string-append who ": path must be string or path"))
      ) ;unless
      (unless (string? pattern)
        (type-error (string-append who ": pattern must be string"))
      ) ;unless
      (map (lambda (entry) (path-join p entry))
        (g_path-glob (path->string p) pattern recursive)
      ) ;map
    ) ;define

    ;; ; 对齐 pathlib.Path.glob(pattern)
    ;; ; 单层目录通配检索，返回包含匹配项的 <path> 记录列表。
    (define (path-glob p pattern)
      (%glob-entries "path-glob" p pattern #f)
    ) ;define

    ;; ; 对齐 pathlib.Path.rglob(pattern)
    ;; ; 递归全目录树通配检索，返回包含匹配项的 <path> 记录列表。
    (define (path-rglob p pattern)
      (%glob-entries "path-rglob" p pattern #t)
    ) ;define

  ) ;begin
) ;define-library
