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

(import (liii check) (liii rich-range) (liii base) (liii lang) (liii error))

(check-set-mode! 'report-failed)

(let1 r (rich-range :inclusive 1 2)
  (check (r 'start) => 1)
  (check (r 'end) => 2)
  (check (r 'step) => 1)
  (check-true (r 'inclusive?))
) ;let1

(let1 r (rich-range :inclusive 1 3 2)
  (check (r 'start) => 1)
  (check (r 'end) => 3)
  (check (r 'step) => 2)
  (check-true (r 'inclusive?))
) ;let1

(check-false ((rich-range :inclusive 1 3) :empty?))
(check-true ((rich-range :inclusive 3 1) :empty?))
(check-catch 'value-error ((rich-range :inclusive 1 3 0) :empty?))

(let* ((r1 (rich-range :inclusive -2 1))
       (r2 (r1 :map (lambda (x) (* x x)))))
  (check r2 => (rich-list (list 4 1 0 1)))
) ;let*

(let1 r (rich-range 0 10 1 #f)
  (check (r :filter even?) => ($ (list 0 2 4 6 8)))
  (check (r :filter (lambda (x) (> x 5))) => ($ (list 6 7 8 9)))
  (check (r :filter (lambda (x) (< x 0))) => ($ (list )))
) ;let1

(let1 r (rich-range 5 1 -1 #t)
  (check (r :filter odd?) => ($ (list 5 3 1)))
) ;let1

(let1 r (rich-range 5 1 1 #t)
  (check (r :filter odd?) => ($ (list )))
) ;let1

(let1 r (rich-range -5 -1 1 #t)
  (check (r :filter odd?) => ($ (list -5 -3 -1)))
) ;let1

(let1 r (rich-range 5 5 -1 #t)
  (check (r :filter odd?) => ($ (list 5)))
  (check (r :filter even?) => ($ (list )))
) ;let1

(check-false ((rich-range :inclusive 1 3) :contains 4))
(check-true ((rich-range :inclusive 1 3) :contains 2))
(check-true ((rich-range :inclusive 3 3) :contains 3))
(check-true ((rich-range :inclusive 10 1 -3) :contains 4))
(check-false ((rich-range :inclusive 10 1 -3) :contains 14))
(check-false ((rich-range :inclusive 10 1 -3) :contains 3))
(check-catch 'value-error ((rich-range :inclusive 10 1 0) :contains 4))
(check-false ((rich-range :inclusive 10 1 -2) :contains 5))
(check-true  ((rich-range :inclusive 10 1 -2) :contains 6))
(check-false ((rich-range 1 3):contains 3))
(check-false ((rich-range 3 3) :contains 3))
(check-false ((rich-range 3 1 -1):contains 1))

(check-report)

