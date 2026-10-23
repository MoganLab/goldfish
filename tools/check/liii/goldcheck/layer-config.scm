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

(define-library (liii goldcheck layer-config)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii string)
    (liii error)
    (liii json)
  ) ;import
  (export layer-domains-parse layer-order-enabled? find-domain-by-name
    domain-allows-dep?
    domain-name-of domain-root-of domain-deps-of domain-layers-of
    layer-name-of layer-libs-of
  ) ;export
  (begin

    (define (%project-root)
      (if (defined? 'g_project-root (rootlet)) (g_project-root) #f)
    ) ;define

    ;; 规则对象：rules.layer-order 内联；或有 "file" 键时从外部
    ;; JSON 文件加载（相对仓库根，如 gf_check_layer_order.json）。
    (define (%layer-rule cfg)
      (let* ((rules (and cfg (not (json-null? cfg)) (json-ref cfg "rules")))
             (rule (and rules (not (json-null? rules)) (json-ref rules "layer-order")))
            ) ;
        (cond
          ((or (not rule) (json-null? rule)) #f)
          ((let ((f (json-ref rule "file"))) (and (string? f) f))
           =>
           (lambda (f)
             (let* ((root (or (%project-root) "."))
                    (path (string-append root "/" f)))
               (if (file-exists? path)
                 (call-with-input-file path read-json)
                 (value-error "layer-order config file not found: ~A" f))
             ) ;let
           ))
          (else rule)
        ) ;cond
      ) ;let*
    ) ;define

    (define (layer-order-enabled? cfg)
      (let ((rule (%layer-rule cfg)))
        (if (and rule (not (json-null? rule)))
          (let ((en (json-ref rule "enabled")))
            (if (boolean? en) en #t))
          #t
        ) ;if
      ) ;let
    ) ;define

    (define (domain-name-of d) (cdr (assq 'name d)))
    (define (domain-root-of d) (cdr (assq 'root d)))
    (define (domain-deps-of d) (cdr (assq 'depends-on d)))
    (define (domain-layers-of d) (cdr (assq 'layers d)))
    (define (layer-name-of l) (cdr (assq 'name l)))
    (define (layer-libs-of l) (cdr (assq 'libs l)))

    (define (find-domain-by-name domains name)
      (let loop ((rest domains))
        (cond ((null? rest) #f)
              ((string=? (domain-name-of (car rest)) name) (car rest))
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%raw-domains cfg)
      (let* ((rule (%layer-rule cfg))
             (ds (and rule (not (json-null? rule)) (json-ref rule "domains")))
            ) ;
        (if (and ds (not (json-null? ds)) (json-array? ds))
          (vector->list ds)
          '()
        ) ;if
      ) ;let*
    ) ;define

    (define (%parse-layer l domain-name)
      (let ((lname (json-ref l "name"))
            (libs (json-ref l "libs")))
        (unless (string? lname)
          (type-error "layer name must be a string: ~S" lname))
        (unless (and (json-array? libs) (> (vector-length libs) 0))
          (value-error "layer ~A of ~A needs a non-empty libs array" lname domain-name))
        (vector-for-each
          (lambda (p)
            (unless (string? p)
              (type-error "lib pattern must be a string: ~S" p)))
          libs)
        (list (cons 'name lname) (cons 'libs libs))
      ) ;let
    ) ;define

    (define (%parse-domain d)
      (let ((name (json-ref d "name"))
            (root (json-ref d "root"))
            (deps (json-ref d "depends-on"))
            (layers (json-ref d "layers")))
        (unless (string? name)
          (type-error "domain name must be a string: ~S" name))
        (unless (string? root)
          (type-error "domain ~A root must be a string" name))
        (unless (and (json-array? layers) (> (vector-length layers) 0))
          (value-error "domain ~A needs a non-empty layers array" name))
        (unless (or (not deps) (json-null? deps) (json-array? deps))
          (type-error "domain ~A depends-on must be an array" name))
        (list (cons 'name name)
          (cons 'root root)
          (cons 'depends-on
            (if (or (not deps) (json-null? deps)) #() deps))
          (cons 'layers
            (vector->list
              (vector-map
                (lambda (l) (%parse-layer l name))
                layers)))
        ) ;list
      ) ;let
    ) ;define

    (define (%check-unique-names! domains)
      (let loop ((seen '()) (rest domains))
        (unless (null? rest)
          (let ((name (domain-name-of (car rest))))
            (if (member name seen)
              (value-error "duplicate domain name: ~A" name)
              (loop (cons name seen) (cdr rest))
            ) ;if
          ) ;let
        ) ;unless
      ) ;let
    ) ;define

    (define (%check-deps-exist! domains)
      (for-each
        (lambda (d)
          (vector-for-each
            (lambda (dep)
              (unless (find-domain-by-name domains dep)
                (value-error
                  "domain ~A depends on unknown domain: ~A"
                  (domain-name-of d) dep)))
            (domain-deps-of d)))
        domains)
    ) ;define

    (define (%check-acyclic-from! domains start)
      (let loop ((queue (list start)) (path '()))
        (unless (null? queue)
          (let ((name (car queue)))
            (if (member name path)
              (value-error "domain dependency cycle via: ~A" name)
              (let ((d (find-domain-by-name domains name)))
                (if d
                  (begin
                    (vector-for-each
                      (lambda (dep) (loop (list dep) (cons name path)))
                      (domain-deps-of d))
                    (loop (cdr queue) path))
                  (loop (cdr queue) path))
              ) ;if
            ) ;let
          ) ;let
        ) ;unless
      ) ;let
    ) ;define

    (define (layer-domains-parse cfg)
      "从 cfg 的 rules.layer-order.domains 解析域表并自检：\ndepends-on 引用存在、域名唯一、域依赖图无环。返回域列表。"
      (let* ((raw (%raw-domains cfg))
             (domains (map %parse-domain raw)))
        (%check-unique-names! domains)
        (%check-deps-exist! domains)
        (for-each
          (lambda (d) (%check-acyclic-from! domains (domain-name-of d)))
          domains)
        domains
      ) ;let*
    ) ;define

    (define (domain-allows-dep? domain domains dep-name)
      "闭包判定：dep-name 属于 domain 的直接或间接依赖。"
      (let loop ((queue (vector->list (domain-deps-of domain))) (seen '()))
        (cond ((null? queue) #f)
              ((string=? (car queue) dep-name) #t)
              ((member (car queue) seen) (loop (cdr queue) seen))
              (else
                (let ((sub (find-domain-by-name domains (car queue))))
                  (loop
                    (append
                      (if sub (vector->list (domain-deps-of sub)) '())
                      (cdr queue))
                    (cons (car queue) seen))))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
