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

(import (liii base)
        (liii check)
        (liii sort)
        (liii trie)
) ;import

(check-set-mode! 'report-failed)

(check-true (trie? (make-trie)))

(let1 trie (make-trie)
  (check-false (trie-ref trie (string->list "hey")))
) ;let1

(let1 trie (make-trie)
  (check-false (trie-ref trie (string->list "hey")))
  (check (trie-ref trie (string->list "hey") 'default) => 'default)
  (check-false (trie-ref* trie (string->list "hey")))
  (check (trie->list trie) => '(()))
) ;let1


(let1 trie (make-trie)
  (trie-insert! trie (string->list "hello") 'world)
  (check (trie-ref trie (string->list "hello")) => 'world)
  (check-false (trie-ref trie (string->list "hell")))
  (check-false (trie-ref trie (string->list "helloo")))

  (trie-insert! trie (string->list "hello") 'scheme)
  (check (trie-ref trie (string->list "hello")) => 'scheme)

  (trie-insert! trie (string->list "hey") 'there)
  (trie-insert! trie (string->list "hi")  'again)
  (check (trie-ref trie (string->list "hey")) => 'there)
  (check (trie-ref trie (string->list "hi"))  => 'again)
  (check-false (trie-ref trie (string->list "h")))


  (check (list-sort! < (trie->list trie))
         => '(((#\h ((#\i () again)                                ; hi
                     (#\e ((#\y () there)                          ; hey
                           (#\l ((#\l ((#\o () scheme)))))))))) ; hello
                     ) ;e
  ) ;check
) ;let1

(let1 trie (make-trie)
  (trie-insert! trie (string->list "apple") 'fruit)
  (trie-insert! trie (string->list "app") 'prefix)
  (trie-insert! trie (string->list "application") 'software)

  (check (trie-ref trie (string->list "app"))         => 'prefix)
  (check (trie-ref trie (string->list "apple"))       => 'fruit)
  (check (trie-ref trie (string->list "application")) => 'software)
  (check-false (trie-ref trie (string->list "appl")))
) ;let1

(let1 trie (make-trie)
  (trie-insert! trie '() 'root)
  (check (trie-ref trie '()) => 'root)
  (check (trie-value trie) => '(root))
  (trie-insert! trie (string->list "a") 'letter)
  (check (trie-ref trie '()) => 'root)
  (check (trie-ref trie (string->list "a")) => 'letter)
) ;let1

(let1 trie (make-trie)
  (check (trie-value trie) => '())
  (trie-insert! trie '() 'root-value)
  (check (trie-value trie) => '(root-value))
  (trie-insert! trie (string->list "test") 'other-value)
  (check (trie-value trie) => '(root-value))
  (check (trie->list trie) => '(((#\t ((#\e ((#\s ((#\t () other-value)))))))) ; "test" trie-value
                                root-value) ; (tire-value trie)
  ) ;check
) ;let1


(check-report)
