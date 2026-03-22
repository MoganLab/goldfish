;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii check)
        (liii comparator)
        (liii hash-table)
        (liii base)
) ;import

(check-set-mode! 'report-failed)

(define empty-ht (make-hash-table))

(let ((ht (make-hash-table)))
  (check (ht 'a) => #f)
  (hash-table-set! ht 'a 1)
  (check (ht 'a) => 1)
) ;let

(let ((ht (make-hash-table (make-default-comparator))))
  (hash-table-set! ht 1 2)
  (check (ht 1) => 2)
) ;let

(let* ((mod10 (lambda (x) (modulo x 10)))
       (digit=? (lambda (x y) (= (modulo x 10) (modulo y 10))))
       (comp (make-comparator number? digit=? #f mod10))
       (ht (make-hash-table comp)))
  (hash-table-set! ht 1 2)
  (hash-table-set! ht 11 3)
  (check (ht 1) => 3)
  (check (ht 11) => 3)
  (check (ht 21) => 3)
) ;let*

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'brand 'liii)
  (check (hash-table-contains? ht 'brand) => #t)
  (hash-table-set! ht 'brand #f)
  (check (hash-table-contains? ht 'brand) => #f)
) ;let

(check (hash-table-empty? empty-ht) => #t)

(let ((test-ht (make-hash-table)))
  (hash-table-set! test-ht 'key 'value)
  (check (hash-table-empty? test-ht) => #f)
) ;let

(let ((empty-h1 (make-hash-table))
      (empty-h2 (make-hash-table)))
  (check (hash-table=? empty-h1 empty-h2) => #t)
) ;let

(let ((t1 (make-hash-table))
      (t2 (make-hash-table)))
  (hash-table-set! t1 'a 1)
  (hash-table-set! t2 'a 1)
  (check (hash-table=? t1 t2) => #t)
  (hash-table-set! t1 'b 2)
  (check (hash-table=? t1 t2) => #f)
) ;let

(check (hash-table-ref empty-ht 'key) => #f)

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'key 'value)
  (check (hash-table-ref ht 'key) => 'value)
  (check (ht 'key) => 'value)
) ;let

(let ((ht (make-hash-table)))
  (check (hash-table-ref/default ht 'key 'value1) => 'value1)
  (check (hash-table-ref/default ht 'key (+ 1 2)) => 3)

  (hash-table-set! ht 'key 'value)
  (check (hash-table-ref/default ht 'key
           (lambda () (begin (display "hello")
                             (+ 1 2)))
           ) ;lambda
    => 'value
  ) ;check
) ;let

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'k1 'v1 'k2 'v2)
  (check (ht 'k1) => 'v1)
  (check (ht 'k2) => 'v2)
) ;let

(let ((ht (make-hash-table)))
  (hash-table-update! ht 'key 'value)
  (check (hash-table-delete! ht 'key) => 1)
  (check-false (hash-table-contains? ht 'key))

  (hash-table-update! ht 'key1 'value1)
  (hash-table-update! ht 'key2 'value2)
  (hash-table-update! ht 'key3 'value3)
  (hash-table-update! ht 'key4 'value4)
  (check (hash-table-delete! ht 'key1 'key2 'key3) => 3)
) ;let

(let ((ht (make-hash-table)))
  (hash-table-update! ht 'key 'value)
  (check (ht 'key) => 'value)
  (hash-table-update! ht 'key 'value1)
  (check (ht 'key) => 'value1)
  (hash-table-update! ht 'key #f)
  (check (ht 'key) => #f)
) ;let

(let ((ht (make-hash-table)))
  (hash-table-update!/default ht 'key1 (lambda (x) (+ x 1)) 10)
  (check (ht 'key1) => 11)
  (hash-table-update!/default ht 'key1 (lambda (x) (+ x 1)) 10)
  (check (ht 'key1) => 12)
  (hash-table-update!/default ht 'key2 (lambda (x) (* x 2)) 5)
  (check (ht 'key2) => 10)
  (hash-table-update!/default ht 'key2 (lambda (x) (+ x 2)) 5)
  (check (ht 'key2) => 12)
  (hash-table-update!/default ht 'key2 (lambda (x) #f) 5)
  (check (ht 'key2) => #f)
) ;let

(let ((ht (make-hash-table)))
  (hash-table-update! ht 'key 'value)
  (hash-table-update! ht 'key1 'value1)
  (hash-table-update! ht 'key2 'value2)
  (hash-table-clear! ht)
  (check-true (hash-table-empty? ht))
) ;let

(check (hash-table-size empty-ht) => 0)

(let ((populated-ht (make-hash-table)))
  (hash-table-set! populated-ht 'key1 'value1)
  (hash-table-set! populated-ht 'key2 'value2)
  (hash-table-set! populated-ht 'key3 'value3)
  (check (hash-table-size populated-ht) => 3)
) ;let

(check (hash-table-keys empty-ht) => '())

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'k1 'v1)
  (check (hash-table-keys ht) => '(k1))
) ;let

(check (hash-table-values empty-ht) => '())

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'k1 'v1)
  (check (hash-table-values ht) => '(v1))
) ;let

(let ((ht (make-hash-table)))
  (check (call-with-values (lambda () (hash-table-entries ht))
                           (lambda (ks vs) (list ks vs)))
         => (list (list ) (list ))
  ) ;check

  (hash-table-set! ht 'k1 'v1)
  (check (call-with-values (lambda () (hash-table-entries ht))
                           (lambda (ks vs) (list ks vs)))
         => (list (list 'k1) (list 'v1))
  ) ;check
) ;let

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'a 1)
  (hash-table-set! ht 'b 2)
  (hash-table-set! ht 'c 3)

  (check (hash-table-find
           (lambda (k v) (= v 2))
           ht
           (lambda () 'not-found))
    => 2
  ) ;check
) ;let

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'a 1)
  (hash-table-set! ht 'b 2)
  (hash-table-set! ht 'c 3)
    
  (check (hash-table-find
          (lambda (k v) (= v 4))
          ht
          'not-found)
         => 'not-found
  ) ;check
) ;let

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'a 1)
  (hash-table-set! ht 'b 2)
  (hash-table-set! ht 'c 3)

  (check (hash-table-find
          (lambda (k v) (eq? k 'b))
          ht
          (lambda () 'not-found))
         => 2
  ) ;check
) ;let

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'a 1)
  (hash-table-set! ht 'b 2)
  (hash-table-set! ht 'c 3)

  (check (hash-table-find
          (lambda (k v) (eq? k 'd))
          ht
          'not-found)
    => 'not-found
  ) ;check
) ;let

(let ((empty-ht (make-hash-table)))
  (check (hash-table-find
          (lambda (k v) #t)
          empty-ht
          'empty)
         => 'empty
  ) ;check
) ;let

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'a 1)
  (hash-table-set! ht 'b 2)
  (hash-table-set! ht 'c 3)
    
  (check (hash-table-find
          (lambda (k v) (and (symbol? k) (even? v)))
          ht
          (lambda () 'not-found))
         => 2
  ) ;check
) ;let

(check (hash-table-count (lambda (k v) #f) (hash-table)) => 0)
(check (hash-table-count (lambda (k v) #t) (hash-table 'a 1 'b 2 'c 3)) => 3)
(check (hash-table-count (lambda (k v) #f) (hash-table 'a 1 'b 2 'c 3)) => 0)

(check (hash-table-count (lambda (k v) (eq? k 'b)) (hash-table 'a 1 'b 2 'c 3)) => 1)

(check (hash-table-count (lambda (k v) (> v 1)) (hash-table 'a 1 'b 2 'c 3)) => 2)

(check (hash-table-count (lambda (k v) (string? k))
                         (hash-table "apple" 1 "banana" 2)) => 2)

(check (hash-table-count (lambda (k v) (and (symbol? k) (even? v)))
                         (hash-table 'apple 2 'banana 3 'cherry 4)) => 2)


(check (hash-table-count (lambda (k v) (eq? k v))
                         (hash-table 'a 'a 'b 'b 'c 'd)) => 2)

(check (hash-table-count (lambda (k v) (number? k))
                         (hash-table 1 100 2 200 3 300)) => 3)

(check (hash-table-count (lambda (k v) (list? v))
                         (hash-table 'a '(1 2) 'b '(3 4) 'c 3)) => 2)

(check (hash-table-count (lambda (k v)  
                           (= (char->integer (string-ref (symbol->string k) 0)) v))
                         (hash-table 'a 97 'b 98 'c 99)) => 3)

(let ((cnt 0))
  (hash-table-for-each
    (lambda (k v)
      (set! cnt (+ cnt v))
    ) ;lambda
    (hash-table 'a 1 'b 2 'c 3)
  ) ;hash-table-for-each
  (check cnt => 6)
) ;let

(let ((ht (hash-table 'a 1 'b 2 'c 3)))
  (check (hash-table-fold (lambda (k v acc) (+ acc v)) 0 ht) => 6)
) ;let

(check (hash-table-fold (lambda (k v acc) (+ acc v)) 10 (hash-table)) => 10)

(let* ((ht (hash-table 'a 1 'b 2 'c 3))
       (ks (hash-table-map->list (lambda (k v) k) ht))
       (vs (hash-table-map->list (lambda (k v) v) ht)))
  (check-true (not (null? (member 'a ks))))
  (check-true (not (null? (member 'b ks))))
  (check-true (not (null? (member 'c ks))))
  (check-true (not (null? (member 1 vs))))
  (check-true (not (null? (member 2 vs))))
  (check-true (not (null? (member 3 vs))))
) ;let*

(let ((ht (make-hash-table)))
  (check (hash-table->alist ht) => (list))
  (hash-table-set! ht 'k1 'v1)
  (check (hash-table->alist ht) => '(k1 v1))
) ;let

(check (hash-table->alist (alist->hash-table (list 'k1 'v1)))
       => (list 'k1 'v1)
) ;check

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'k1 'v1)
  (hash-table-set! ht 'k2 'v2)
  (let ((ht-copy (hash-table-copy ht)))
    ;; 复制后内容一致
    (check (hash-table->alist ht-copy) => (hash-table->alist ht))
    ;; 修改副本不影响原表
    (hash-table-set! ht-copy 'k1 'modified)
    (check (hash-table-ref ht 'k1) => 'v1)
    (check (hash-table-ref ht-copy 'k1) => 'modified)
    ;; 原表无此键，副本有
    (hash-table-set! ht-copy 'k3 'v3)
    (check (hash-table-ref ht 'k3) => #f)
    (check (hash-table-ref ht-copy 'k3) => 'v3)
  ) ;let
) ;let

(check-report)
