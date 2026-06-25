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

;; 语言处理器抽象层：把"按语言收集文件、格式化、逐文件检查"做成通用框架，
;; 各语言（scheme-fmt / cpp-fmt / 未来的 python-fmt ...）只实现一组同名过程并注册进来。
;;
;; handler 是一个关联列表，键为符号。统一协议：
;;   ((name         . 'scheme)
;;    (label        . "Scheme")                 ; 人类可读名，用于日志
;;    (extensions   . (".scm" ".sld"))          ; 该语言处理的后缀列表（带点）
;;    (collect      . (lambda (cfg) ...))       ; 从 cfg 的 path 收集文件列表（仓库批量）
;;    (format-files . (lambda (files cfg) ...)) ; 批量格式化，返回 (total updated cached)
;;    (format-file  . (lambda (path dry-run excludes) ...))        ; 单文件格式化
;;    (format-directory . (lambda (dir exts excludes dry-run) ...)); 目录递归格式化
;;    (check-file   . (lambda (path cfg) ...))) ; 单文件检查 -> #t(已格式化)/#f
;; 仓库批量/check 的方法接 cfg（handler 内部用 goldfmt-config 访问器自取本语言配置）；
;; 单文件/目录的方法接调用方传入的后缀/excludes（带路径参数模式无完整 cfg）。
;; 主入口按文件后缀查注册表派发，不硬编码任何语言。新增语言只需注册 handler。
;; 通用 exclude 匹配（精确 + glob *）与文件收集也在此提供，供各 handler 复用。

(define-library (liii goldfmt-lang)
  (import (liii base) (liii path) (liii string))
  (export register-lang!
    lang-list
    lang-ref
    lang-name
    lang-label
    lang-extensions
    extensions-for-lang-name
    lang-for-extension
    lang-for-extensions
    lang-for-name
    path-matches-exclude?
    file-excluded?
    collect-files
  ) ;export
  (begin

    ;; ---- 注册表 ---------------------------------------------------------
    ;; 内部可变状态：handler 列表，按注册（模块 import）先后顺序排列。
    ;; 主入口仓库批量 / check 时遍历此列表派发；新增语言只需 import 新模块即自动加入。
    (define %lang-registry '())

    ;; 追加到末尾（保持 import 顺序）；同名 handler 替换旧的。
    (define (register-lang! handler)
      (let ((name (cdr (assq 'name handler))))
        (set! %lang-registry
          (let loop
            ((rs %lang-registry) (acc '()))
            (if (null? rs)
              (reverse (cons handler acc))
              (if (eq? (lang-name (car rs)) name)
                (loop (cdr rs) (cons handler acc))
                (loop (cdr rs) (cons (car rs) acc))
              ) ;if
            ) ;if
          ) ;let
        ) ;set!
        handler
      ) ;let
    ) ;define

    ;; 按注册顺序返回 handler 列表。
    (define (lang-list)
      %lang-registry
    ) ;define

    ;; handler 的 name 字段（符号）。
    (define (lang-name handler)
      (cdr (assq 'name handler))
    ) ;define

    ;; handler 的 label 字段（人类可读名）；未提供时回退为 name。
    (define (lang-label handler)
      (let ((p (assq 'label handler)))
        (if p (cdr p) (lang-name handler))
      ) ;let
    ) ;define

    ;; handler 字段访问器：取某方法。
    (define (lang-ref handler key)
      (cdr (assq key handler))
    ) ;define

    ;; handler 的 extensions 字段（后缀列表，带点）。
    (define (lang-extensions handler)
      (cdr (assq 'extensions handler))
    ) ;define

    ;; token（如 "cpp"/"scheme"）若是已注册语言名，返回该语言后缀列表；否则 #f。
    ;; 供 -e 按语言名展开（-e cpp => cpp 语言全部后缀）。
    (define (extensions-for-lang-name token)
      (let loop
        ((handlers (lang-list)))
        (if (null? handlers)
          #f
          (if (eq? (lang-name (car handlers)) (string->symbol token))
            (lang-extensions (car handlers))
            (loop (cdr handlers))
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    ;; ext（单个带点后缀，如 ".cpp"）是否由 handler 接管。
    (define (handler-matches-ext? handler ext)
      (let loop
        ((exts (lang-extensions handler)))
        (if (null? exts) #f (if (string=? (car exts) ext) #t (loop (cdr exts))))
      ) ;let
    ) ;define

    ;; 按单个后缀查 handler：遍历注册表返回第一个 extensions 含 ext 的 handler；
    ;; 都不匹配则返回 #f（调用方自行决定默认/报错）。
    (define (lang-for-extension ext)
      (let loop
        ((handlers (lang-list)))
        (if (null? handlers)
          #f
          (if (handler-matches-ext? (car handlers) ext)
            (car handlers)
            (loop (cdr handlers))
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    ;; 按后缀列表查 handler：返回 extensions 与 exts 有交集的 handler 列表
    ;; （目录模式可能 -e 指定多语言后缀）。
    (define (lang-for-extensions exts)
      (let loop
        ((handlers (lang-list)) (acc '()))
        (if (null? handlers)
          (reverse acc)
          (let ((h (car handlers)))
            (loop (cdr handlers)
              (if (let any
                    ((es exts))
                    (if (null? es) #f (if (handler-matches-ext? h (car es)) #t (any (cdr es))))
                  ) ;let
                (cons h acc)
                acc
              ) ;if
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 按语言名（符号）查 handler：返回第一个 name 匹配的 handler，无则 #f。
    (define (lang-for-name name)
      (let loop
        ((handlers (lang-list)))
        (if (null? handlers)
          #f
          (if (eq? (lang-name (car handlers)) name) (car handlers) (loop (cdr handlers)))
        ) ;if
      ) ;let
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
