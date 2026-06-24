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

;; 语言处理器抽象层：把"按语言收集文件、批量格式化、逐文件检查"做成通用框架，
;; 各语言（scheme-fmt / cpp-fmt）只实现一组同名过程并注册进来。
;;
;; handler 是一个关联列表，键为过程名符号，值为该语言对应的过程：
;;   ((name        . 'scheme)
;;    (collect     . (lambda (paths suffixes excludes) ...))   ; 递归收集文件列表
;;    (format-file . (lambda (path dry-run?) ...))            ; 单文件格式化
;;    (check-file  . (lambda (path) ...)))                    ; 单文件检查 -> #t/#f
;; 通用 exclude 匹配（精确 + glob *）也在此提供，供 collect 复用。

(define-library (liii goldfmt-lang)
  (import (liii base) (liii path) (liii string))
  (export register-lang!
    get-lang
    all-langs
    path-matches-exclude?
    file-excluded?
    collect-files
  ) ;export
  (begin

    ;; ---- 注册表 ---------------------------------------------------------
    ;; 内部可变状态：语言名 -> handler 关联列表。
    (define %lang-registry '())

    (define (register-lang! handler)
      (let ((name (cdr (assq 'name handler))))
        (set! %lang-registry
          (cons (cons name handler)
            (let loop
              ((rs %lang-registry) (acc '()))
              (if (null? rs)
                (reverse acc)
                (loop (cdr rs) (if (eq? (car (car rs)) name) acc (cons (car rs) acc)))
              ) ;if
            ) ;let
          ) ;cons
        ) ;set!
        handler
      ) ;let
    ) ;define

    (define (get-lang name)
      (let ((p (assq name %lang-registry)))
        (if p (cdr p) #f)
      ) ;let
    ) ;define

    ;; 按注册逆序返回 handler 列表（scheme-fmt、cpp-fmt 各自注册后即可遍历）。
    (define (all-langs)
      (map cdr %lang-registry)
    ) ;define

    ;; handler 字段访问器。
    (define (lang-ref handler key)
      (cdr (assq key handler))
    ) ;define

    ;; ---- exclude 匹配（迁移自 goldformat-path.scm）---------------------
    ;; 把路径分隔符统一成正斜杠。
    (define (normalize-sep s)
      (string-replace s "\\" "/")
    ) ;define

    (define (has-wildcard? s)
      (let loop
        ((i (- (string-length s) 1)))
        (if (< i 0) #f (if (char=? (string-ref s i) #\*) #t (loop (- i 1))))
      ) ;let
    ) ;define

    (define (string-index-of str sub start)
      (let ((sublen (string-length sub)) (slen (string-length str)))
        (let loop
          ((i start))
          (cond ((> (+ i sublen) slen) #f)
                ((string=? (substring str i (+ i sublen)) sub) i)
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (string-starts-with? str prefix)
      (and (>= (string-length str) (string-length prefix))
        (string=? (substring str 0 (string-length prefix)) prefix)
      ) ;and
    ) ;define

    ;; 简单 glob：只支持 *（匹配任意字符序列）。pattern 不含 * 时按精确匹配。
    (define (glob-match? pattern str)
      (if (not (has-wildcard? pattern))
        (string=? pattern str)
        (let ((parts (string-split pattern "*")) (ends-with-star (string-ends? pattern "*")))
          (let loop
            ((ps parts) (pos 0) (first? #t))
            (cond ((null? ps) (or ends-with-star (= pos (string-length str))))
                  (else (let ((seg (car ps)))
                          (cond ((string=? seg "") (loop (cdr ps) pos #f))
                                (first? (if (string-starts-with? str seg) (loop (cdr ps) (string-length seg) #f) #f)
                                ) ;first?
                                (else (let ((found (string-index-of str seg pos)))
                                        (if found (loop (cdr ps) (+ found (string-length seg)) #f) #f)
                                      ) ;let
                                ) ;else
                          ) ;cond
                        ) ;let
                  ) ;else
            ) ;cond
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
      (let loop
        ((pats excludes))
        (if (null? pats)
          #f
          (if (path-matches-exclude? entry-str (car pats)) #t (loop (cdr pats)))
        ) ;if
      ) ;let
    ) ;define

    ;; ---- 通用文件收集 ---------------------------------------------------
    ;; 递归收集 dir-path 下、后缀命中 suffixes、未被 excludes 排除的文件。
    ;; suffixes 为 '() 时不按后缀过滤（交由语言自行判断，但通用层这里假定已给后缀）。
    (define (suffix-match? name suffixes)
      (let loop
        ((exts suffixes))
        (if (null? exts) #f (if (string-ends? name (car exts)) #t (loop (cdr exts))))
      ) ;let
    ) ;define

    (define (collect-files dir-path suffixes excludes)
      (let ((entries (path-list-path (path dir-path))))
        (let loop
          ((i 0) (acc '()))
          (if (>= i (vector-length entries))
            acc
            (let ((entry (vector-ref entries i)))
              (cond ((path-file? entry)
                     (let ((s (path->string entry)))
                       (if (and (suffix-match? s suffixes) (not (file-excluded? s excludes)))
                         (loop (+ i 1) (cons s acc))
                         (loop (+ i 1) acc)
                       ) ;if
                     ) ;let
                    ) ;
                    ((path-dir? entry)
                     (let ((d (path->string entry)))
                       (if (file-excluded? d excludes)
                         (loop (+ i 1) acc)
                         (loop (+ i 1) (append (collect-files d suffixes excludes) acc))
                       ) ;if
                     ) ;let
                    ) ;
                    (else (loop (+ i 1) acc))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
