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
  (export %locate layer-name-of-lib domain-name-of-lib
    %locate-layer-in %domain-for-path
  ) ;export
  (begin

    ;; 家族前缀继承："liii goldfmt" 登记则 "liii goldfmt xxx" 同层
    (define (%lib-under? name lib)
      (or (string=? name lib)
          (string-starts-with? lib (string-append name " ")))
    ) ;define

    (define (%locate-layer layers lib idx)
      (if (null? layers)
        #f
        (if
          (let scan
            ((names (vector->list (layer-libs-of (car layers)))))
            (cond ((null? names) #f)
                  ((%lib-under? (car names) lib) #t)
                  (else (scan (cdr names)))))
          (cons (car layers) idx)
          (%locate-layer (cdr layers) lib (+ idx 1))
        ) ;if
      ) ;if
    ) ;define

    ;; 穷举匹配：库名在域表中首个命中的 (domain layer idx)
    (define (%locate domains lib)
      (let loop-d ((ds domains))
        (if (null? ds)
          #f
          (let ((hit (%locate-layer (domain-layers-of (car ds)) lib 0)))
            (if hit
              (list (car ds) (car hit) (cdr hit))
              (loop-d (cdr ds))
            ) ;if
          ) ;let
        ) ;if
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

    ;; 在指定域内找库名所在层（家族前缀继承），返回 (layer . idx) 或 #f
    (define (%locate-layer-in domains domain lib)
      (let loop
        ((layers (domain-layers-of domain)) (idx 0))
        (cond ((null? layers) #f)
              ((let scan
                 ((names (vector->list (layer-libs-of (car layers)))))
                 (cond ((null? names) #f)
                       ((%lib-under? (car names) lib) #t)
                       (else (scan (cdr names)))))
               (cons (car layers) idx)
              ) ;
              (else (loop (cdr layers) (+ idx 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%root-segs root)
      (let* ((norm (normalize-sep root))
             (norm2
               (if (string-ends? norm "/")
                 (substring norm 0 (- (string-length norm) 1))
                 norm
               ) ;if
             ))
        (string-split norm2 "/")
      ) ;let
    ) ;define

    (define (%prefix-at? rsegs segs start)
      (let loop ((i 0))
        (or (= i (length rsegs))
            (and (< (+ start i) (length segs))
                 (string=? (list-ref rsegs i) (list-ref segs (+ start i)))
                 (loop (+ i 1)))
        ) ;or
      ) ;let
    ) ;define

    (define (%seg-match? root segs)
      (let ((rsegs (%root-segs root)))
        (let scan ((start 0))
          (cond ((> start (- (length segs) (length rsegs))) #f)
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
