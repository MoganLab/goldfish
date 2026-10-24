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

(define-library (liii goldfmt path)
  (import (liii base) (liii path) (liii string) (liii list))
  (export normalize-sep has-wildcard?
    glob-match? path-matches-exclude? file-excluded? suffix-match? collect-files
    collect-files-dfs
  ) ;export
  (begin

    (define (normalize-sep s)
      (string-replace s "\\" "/")
    ) ;define

    (define (has-wildcard? s)
      (string-contains? s "*")
    ) ;define

    (define (match-glob-seg ps str pos first?)
      (if (null? ps)
        pos
        (let ((seg (car ps)))
          (cond ((string=? seg "") (match-glob-seg (cdr ps) str pos #f))
                (first? (if (string-starts? str seg)
                          (match-glob-seg (cdr ps) str (string-length seg) #f)
                          #f
                        ) ;if
                ) ;first?
                (else
                  (let ((found (string-position seg str pos)))
                    (if found (match-glob-seg (cdr ps) str (+ found (string-length seg)) #f) #f)
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    ;; 简单 glob：只支持 *（匹配任意字符序列）。pattern 不含 * 时按精确匹配。
    (define (glob-match? pattern str)
      (if (not (has-wildcard? pattern))
        (string=? pattern str)
        (let ((parts (string-split pattern "*")) (ends-with-star (string-ends? pattern "*")))
          (let ((pos (match-glob-seg parts str 0 #t)))
            (and pos (or ends-with-star (= pos (string-length str))))
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    ;; entry 是否命中单个 exclude pattern：
    ;;   pattern 含通配符 *：glob 匹配。含 / 时对完整路径匹配，否则对 basename 匹配。
    ;;   pattern 不含通配符：精确路径匹配（等于 pattern，或以 /pattern 结尾）。
    (define (path-matches-exclude? entry-str pattern)
      (let ((entry-norm (normalize-sep entry-str)) (p (normalize-sep pattern)))
        (if (has-wildcard? p)
          (glob-match? p
            (if (string-contains? p "/") entry-norm (path-name (path entry-norm)))
          ) ;glob-match?
          (let ((plen (string-length p)) (elen (string-length entry-norm)))
            (or (string=? entry-norm p)
              (and (>= elen (+ plen 1))
                (char=? (string-ref entry-norm (- elen plen 1)) #\/)
                (string-ends? entry-norm p)
              ) ;and
            ) ;or
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; entry 是否命中 excludes 列表中任一 pattern。
    (define (file-excluded? entry-str excludes)
      (any (lambda (pat) (path-matches-exclude? entry-str pat)) excludes)
    ) ;define

    ;; ---- 通用文件收集 ---------------------------------------------------
    (define (suffix-match? name suffixes)
      (any (lambda (ext) (string-ends? name ext)) suffixes)
    ) ;define

    (define (collect-files dir-path suffixes excludes)
      (let ((entries (path-list-path (path dir-path))))
        (fold
          (lambda (entry acc)
            (cond
             ((path-file? entry)
              (let ((s (path->string entry)))
                (if (and (suffix-match? s suffixes) (not (file-excluded? s excludes)))
                  (cons s acc)
                  acc
                ) ;if
              ) ;let
             ) ;
             ((path-dir? entry)
              (let ((d (path->string entry)))
                (if (file-excluded? d excludes)
                  acc
                  (append (collect-files d suffixes excludes) acc)
                ) ;if
              ) ;let
             ) ;
             (else acc)
            ) ;cond
          ) ;lambda
          '()
          (vector->list entries)
        ) ;fold
      ) ;let
    ) ;define

    (define (collect-files-dfs dir-path suffixes excludes)
      (reverse (collect-files dir-path suffixes excludes))
    ) ;define

  ) ;begin
) ;define-library
