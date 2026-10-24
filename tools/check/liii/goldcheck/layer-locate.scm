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

(define-library (liii goldcheck layer-locate)
  (import (scheme base)
    (liii base)
    (liii string)
    (liii goldcheck path)
    (liii goldcheck layer-config)
  ) ;import
  (export %locate layer-name-of-lib domain-name-of-lib %locate-layer-in
    %domain-for-path
  ) ;export
  (begin

    ;; 家族前缀继承：“liii goldfmt” 登记则 “liii goldfmt xxx” 同层
    ;; 通配：pattern 末尾 ". *" 表示该前缀下任意子库，如 "liii *" 匹配
    ;; "liii string"，但不匹配 "liii" 本身。
    (define (%wildcard-prefix name)
      (if (and (string-ends? name " *") (> (string-length name) 2))
        (substring name 0 (- (string-length name) 2))
        #f
      ) ;if
    ) ;define

    (define (%lib-under? name lib)
      (let ((prefix (%wildcard-prefix name)))
        (if prefix
          (string-starts-with? lib (string-append prefix " "))
          (or (string=? name lib) (string-starts-with? lib (string-append name " ")))
        ) ;if
      ) ;let
    ) ;define

    ;; 特异性：pattern 字面长度（通配取前缀长度）。
    ;; 越长越特异，使 "liii goldcheck" 胜过 "liii *"。
    (define (%pattern-specificity name)
      (string-length (or (%wildcard-prefix name) name))
    ) ;define

    ;; 在一层的 libs 中找匹配该库的最特异 pattern，返回其特异性或 -1
    (define (%layer-match-score layer lib)
      (let loop
        ((names (vector->list (layer-libs-of layer))) (best -1))
        (cond ((null? names) best)
              ((not (%lib-under? (car names) lib)) (loop (cdr names) best))
              (else
                (loop (cdr names) (max best (%pattern-specificity (car names))))
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; 穷举匹配：在全部域/层中找最特异命中，返回 (domain layer idx) 或 #f。
    ;; 按特异性全局择优（而非按域顺序首个命中），保证 "liii goldcheck"
    ;; 胜过 goldfish 域的 "liii *" 兜底；同特异性时保持先出现者优先。
    (define (%locate domains lib)
      (let loop-d
        ((ds domains) (best #f) (best-score -1))
        (cond ((null? ds) best)
              (else
                (let loop-l
                  ((layers (domain-layers-of (car ds))) (idx 0) (b best) (bs best-score))
                  (cond ((null? layers) (loop-d (cdr ds) b bs))
                        (else
                          (let ((score (%layer-match-score (car layers) lib)))
                            (if (> score bs)
                              (loop-l (cdr layers) (+ idx 1) (list (car ds) (car layers) idx) score)
                              (loop-l (cdr layers) (+ idx 1) b bs)
                            ) ;if
                          ) ;let
                        ) ;else
                  ) ;cond
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (layer-name-of-lib domains lib)
      (let ((loc (%locate domains lib)))
        (if loc (layer-name-of (cadr loc)) #f)
      ) ;let
    ) ;define

    (define (domain-name-of-lib domains lib)
      (let ((loc (%locate domains lib)))
        (if loc (domain-name-of (car loc)) #f)
      ) ;let
    ) ;define

    ;; 在指定域内找库名所在层（家族前缀继承 + 通配），返回 (layer . idx) 或 #f
    (define (%locate-layer-in domains domain lib)
      (let loop
        ((layers (domain-layers-of domain)) (idx 0))
        (cond ((null? layers) #f)
              ((let scan
                 ((names (vector->list (layer-libs-of (car layers)))))
                 (cond ((null? names) #f)
                       ((%lib-under? (car names) lib) #t)
                       (else (scan (cdr names)))
                 ) ;cond
               ) ;let
               (cons (car layers) idx)
              ) ;
              (else (loop (cdr layers) (+ idx 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%root-segs root)
      (let* ((norm (normalize-sep root))
             (norm2
               (if (string-ends? norm "/") (substring norm 0 (- (string-length norm) 1)) norm)
             ) ;norm2
            ) ;
        (string-split norm2 "/")
      ) ;let*
    ) ;define

    (define (%prefix-at? rsegs segs start)
      (let loop
        ((i 0))
        (or (= i (length rsegs))
          (and (< (+ start i) (length segs))
            (string=? (list-ref rsegs i) (list-ref segs (+ start i)))
            (loop (+ i 1))
          ) ;and
        ) ;or
      ) ;let
    ) ;define

    (define (%seg-match? root segs)
      (let ((rsegs (%root-segs root)))
        (let scan
          ((start 0))
          (cond
           ((> start (- (length segs) (length rsegs))) #f)
           ((%prefix-at? rsegs segs start) #t)
           (else (scan (+ start 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (%domain-for-path domains path)
      "按 root 段序列在路径段中的位置定位文件所属域。"
      (let ((segs (string-split (normalize-sep path) "/")))
        (let loop
          ((rest domains))
          (cond ((null? rest) #f)
                ((%seg-match? (domain-root-of (car rest)) segs) (car rest))
                (else (loop (cdr rest)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
