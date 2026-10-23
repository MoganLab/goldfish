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

(define-library (liii goldcheck layer)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii list)
    (liii string)
    (liii goldcheck path)
    (liii goldcheck config)
    (liii goldcheck layer-config)
    (liii goldcheck layer-locate)
  ) ;import
  (export path->lib-name file-imports check-layer-order)
  (begin

    ;; ---- 库名推导 -------------------------------------------------

    (define (%drop-scm-ext s)
      (let ((n (string-length s)))
        (if (string-ends? s ".scm") (substring s 0 (- n 4)) s)
      ) ;let
    ) ;define

    (define (%join-space parts)
      (let loop
        ((rest parts) (acc '()))
        (if (null? rest)
          (apply string-append (reverse acc))
          (loop (cdr rest)
            (if (null? acc) (list (car rest)) (cons (car rest) (cons " " acc)))
          ) ;loop
        ) ;if
      ) ;let
    ) ;define

    (define (path->lib-name path)
      "按目录约定推导库名：在路径段中寻找结构锚点——\n goldfish/liii|srfi|scheme/... 或 tools/T/liii/...，\n 锚点之后、文件名之前的段构成库名。\n 绝对路径与相对路径均支持；非库文件返回 #f。"
      (let* ((norm (normalize-sep path))
             (raw (if (string-ends? norm ".scm") (string-split norm "/") #f))
             (segs
               (if raw
                 (reverse
                   (cons (%drop-scm-ext (car (reverse raw))) (cdr (reverse raw)))
                 ) ;reverse
                 #f
               ) ;if
             ) ;segs
            ) ;
        (if (not segs)
          #f
          (let loop
            ((i 0))
            (cond ((>= i (length segs)) #f)
                  ((not (member (list-ref segs i) '("liii" "srfi" "scheme"))) (loop (+ i 1)))
                  ((and (> i 0) (string=? (list-ref segs (- i 1)) "goldfish"))
                   (%join-space (list-tail segs i))
                  ) ;
                  ((and (> i 1) (string=? (list-ref segs (- i 2)) "tools"))
                   (%join-space (list-tail segs i))
                  ) ;
                  (else (loop (+ i 1)))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    ;; ---- import 提取 -------------------------------------------------

    ;; ---- import 提取（read 解析，非文本扫描）-------------------------

    (define (%join-syms syms)
      (let loop
        ((rest syms) (acc '()))
        (if (null? rest)
          (apply string-append (reverse acc))
          (loop (cdr rest)
            (if (null? acc)
              (list (symbol->string (car rest)))
              (cons (symbol->string (car rest)) (cons " " acc))
            ) ;if
          ) ;loop
        ) ;if
      ) ;let
    ) ;define

    ;; import 子句内容 -> 库名字符串列表（跳过 only/except/rename 等修饰）
    (define (%lib-names items)
      (let loop
        ((rest items) (acc '()))
        (cond ((null? rest) (reverse acc))
              ((and (pair? (car rest)) (memq (car (car rest)) '(liii srfi
                                                                 scheme)))
               (loop (cdr rest) (cons (%join-syms (car rest)) acc))
              ) ;
              (else (loop (cdr rest) acc))
        ) ;cond
      ) ;let
    ) ;define

    (define (%collect-imports clauses)
      (let loop
        ((rest clauses) (acc '()))
        (cond ((null? rest) (reverse acc))
              ((and (pair? (car rest)) (eq? (caar rest) 'import))
               (loop (cdr rest) (append (reverse (%lib-names (cdar rest))) acc))
              ) ;
              (else (loop (cdr rest) acc))
        ) ;cond
      ) ;let
    ) ;define

    (define (file-imports path)
      "read 解析 define-library，返回 import 块的库名列表（含 scheme）。"
      (let ((port (open-input-file path)))
        (dynamic-wind (lambda () #f)
          (lambda ()
            (let ((form (read port)))
              (if (and (pair? form) (eq? (car form) 'define-library))
                (%collect-imports (cdr form))
                '()
              ) ;if
            ) ;let
          ) ;lambda
          (lambda () (close-input-port port))
        ) ;dynamic-wind
      ) ;let
    ) ;define

    ;; ---- 定位 --------------------------------------------------------

    (define (%violation path detail)
      (list 'layer-order (relativize-path path) detail)
    ) ;define

    (define (%import-violation file-path self-domain self-layer self-idx domains imp)
      "判定单个 import：同域不向上放行（同层交由环检测）、\n跨域须在 depends-on 闭包内、未登记报 unknown。"
      (let ((il (%locate domains imp)))
        (cond ((not il) (%violation file-path (string-append "unknown lib: (" imp ")")))
              ((string=? (domain-name-of (car il)) (domain-name-of self-domain))
               (and (not (<= (caddr il) self-idx))
                 (%violation file-path
                   (string-append "imports ("
                     imp
                     ") "
                     (layer-name-of (cadr il))
                     " from "
                     (layer-name-of self-layer)
                   ) ;string-append
                 ) ;%violation
               ) ;and
              ) ;
              ((domain-allows-dep? self-domain domains (domain-name-of (car il))) #f)
              (else
                (%violation file-path
                  (string-append "cross-domain: ("
                    imp
                    ") domain '"
                    (domain-name-of (car il))
                    "' not declared"
                  ) ;string-append
                ) ;%violation
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (%loop-imports file-path self-domain self-layer self-idx domains imports)
      (let loop
        ((rest imports) (acc '()))
        (if (null? rest)
          (reverse acc)
          (let ((v (%import-violation file-path self-domain self-layer self-idx domains (car rest))
                ) ;v
               ) ;
            (loop (cdr rest) (if v (cons v acc) acc))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%check-file-in-domain file-path cfg domains self)
      (let ((self-domain (%domain-for-path domains file-path)))
        (cond ((not self-domain) '())
              ((not (%locate-layer-in domains self-domain self))
               (list (%violation file-path (string-append "lib not registered: " self)))
              ) ;
              (else
                (let ((loc (%locate-layer-in domains self-domain self)))
                  (let ((self-layer (car loc)) (self-idx (cdr loc)))
                    (let ((imports
                            (filter (lambda (l) (not (string-starts-with? l "scheme ")))
                              (file-imports file-path)
                            ) ;filter
                          ) ;imports
                         ) ;
                      (%loop-imports file-path self-domain self-layer self-idx domains imports)
                    ) ;let
                  ) ;let
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (check-layer-order file-path cfg)
      "对单文件执行 layer-order 检查，返回违规列表。\n三步判定：同域向下 / 跨域声明 / 其余 unknown-lib。"
      (cond ((not (layer-order-enabled? cfg)) '())
            ((not (path->lib-name file-path)) '())
            ((string-starts-with? (path->lib-name file-path) "scheme ") '())
            (else
              (let* ((self (path->lib-name file-path)) (domains (layer-domains-parse cfg)))
                (if (null? domains) '() (%check-file-in-domain file-path cfg domains self))
              ) ;let*
            ) ;else
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
