;
; Copyright (C) 2025 The Goldfish Scheme Authors
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

(import (liii base) (liii rich-set) (liii check))

(check-set-mode! 'report-failed)

;; Test factory methods
(check ((rich-hash-set :empty) :size) => 0)
(check ((rich-hash-set :empty) :empty?) => #t)

;; Test basic operations
(let ((ht (make-hash-table)))
  (hash-table-set! ht 'a #t)
  (hash-table-set! ht 'b #t)
  (hash-table-set! ht 'c #t)
  (check ((rich-hash-set ht) :size) => 3)
) ;let

(let ((ht (make-hash-table)))
  (check ((rich-hash-set ht) :empty?) => #t)
  (hash-table-set! ht 'a #t)
  (check ((rich-hash-set ht) :empty?) => #f)
) ;let

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'a #t)
  (hash-table-set! ht 'b #t)
  (check ((rich-hash-set ht) :contains 'a) => #t)
  (check ((rich-hash-set ht) :contains 'c) => #f)
) ;let

;; Test non-destructive operations
(let ((ht (make-hash-table)))
  (hash-table-set! ht 'a #t)
  (hash-table-set! ht 'b #t)
  (let ((s (rich-hash-set ht)))
    (check (s :add-one 'c) => (let ((new-ht (make-hash-table)))
                                (hash-table-set! new-ht 'a #t)
                                (hash-table-set! new-ht 'b #t)
                                (hash-table-set! new-ht 'c #t)
                                (rich-hash-set new-ht))
    ) ;check
    (check (s :add-one 'd) => (let ((new-ht (make-hash-table)))
                                (hash-table-set! new-ht 'a #t)
                                (hash-table-set! new-ht 'b #t)
                                (hash-table-set! new-ht 'd #t)
                                (rich-hash-set new-ht))
    ) ;check
  ) ;let
) ;let

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'a #t)
  (hash-table-set! ht 'b #t)
  (let ((s (rich-hash-set ht)))
    (check (s :remove 'a) => (let ((new-ht (make-hash-table)))
                              (hash-table-set! new-ht 'b #t)
                              (rich-hash-set new-ht))
    ) ;check
    (check (s :remove 'b) => (let ((new-ht (make-hash-table)))
                              (hash-table-set! new-ht 'a #t)
                              (rich-hash-set new-ht))
    ) ;check
  ) ;let
) ;let

;; Test destructive operations
(let ((ht (make-hash-table)))
  (hash-table-set! ht 'a #t)
  (hash-table-set! ht 'b #t)
  (let ((s (rich-hash-set ht)))
    (check (s :add-one! 'c) => (let ((new-ht (make-hash-table)))
                                (hash-table-set! new-ht 'a #t)
                                (hash-table-set! new-ht 'b #t)
                                (hash-table-set! new-ht 'c #t)
                                (rich-hash-set new-ht))
    ) ;check
    (check (s :add-one! 'd) => (let ((new-ht (make-hash-table)))
                                (hash-table-set! new-ht 'a #t)
                                (hash-table-set! new-ht 'b #t)
                                (hash-table-set! new-ht 'c #t)
                                (hash-table-set! new-ht 'd #t)
                                (rich-hash-set new-ht))
    ) ;check
  ) ;let
) ;let

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'a #t)
  (hash-table-set! ht 'b #t)
  (let ((s (rich-hash-set ht)))
    (check (s :remove! 'a) => (let ((new-ht (make-hash-table)))
                                (hash-table-set! new-ht 'b #t)
                                (rich-hash-set new-ht))
    ) ;check
    (check (s :remove! 'b) => (let ((new-ht (make-hash-table)))
                                (rich-hash-set new-ht))
    ) ;check
  ) ;let
) ;let

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'a #t)
  (hash-table-set! ht 'b #t)
  (let ((s (rich-hash-set ht)))
    (check (s :clear!) => (rich-hash-set (make-hash-table)))
  ) ;let
) ;let

(check-report)

