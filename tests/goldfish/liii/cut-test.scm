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
        (liii cut)
) ;import

(check-set-mode! 'report-failed)

(check ((cut list <> 'y <>) 'x 'z) => '(x y z))
(check ((cut + 1 <...>) 2 3) => 6)
(check ((cut + 1 <...>)) => 1)
(check ((cut <> 1 <...>) + 2 3) => 6)
(check ((cut list <> <> <...>) 1 2 3) => '(1 2 3))
(check ((cut list <> <> <...>) 1 2) => '(1 2))
(check ((cut + 1 2)) => 3)
(check ((cut <>) list) => ())
(check ((cut)) => ())
(check ((cut <> #t <...>) if 1 0) => 1)

(check-catch 'wrong-number-of-args ((cut list <> <>) 1))
(check-catch 'wrong-number-of-args ((cut list <> <> <...>) 1))
(check-catch 'syntax-error ((cut list <> <> <...> <>) 1 2 3))

(let* ((a 1)
       (f (cut <> (set! a 2))))
  (check a => 1)
  (check (f (lambda (x) x)) => 2)
  (check a => 2)
) ;let*

(let* ((a 1)
       (f (cute <> (set! a 2))))
  (check a => 2)
  (set! a 1)
  (check (f (lambda (x) x)) => 2)
  (check a => 1)
) ;let*

(check-report)

