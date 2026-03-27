;
; Copyright (C) 2026 The Goldfish Scheme Authors
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
        (scheme cxr)
) ;import

(check-set-mode! 'report-failed)

(define sample-cxr-3
  '(((a11 a12 a13 a14)
     (a21 a22 a23 a24)
     (a31 a32 a33 a34)
     (a41 a42 a43 a44))
    ((b11 b12 b13 b14)
     (b21 b22 b23 b24)
     (b31 b32 b33 b34)
     (b41 b42 b43 b44))
    ((c11 c12 c13 c14)
     (c21 c22 c23 c24)
     (c31 c32 c33 c34)
     (c41 c42 c43 c44))
    ((d11 d12 d13 d14)
     (d21 d22 d23 d24)
     (d31 d32 d33 d34)
     (d41 d42 d43 d44)))
) ;define

(define sample-cxr-4
  '((((a111 a112 a113 a114)
      (a121 a122 a123 a124)
      (a131 a132 a133 a134)
      (a141 a142 a143 a144))
     ((a211 a212 a213 a214)
      (a221 a222 a223 a224)
      (a231 a232 a233 a234)
      (a241 a242 a243 a244))
     ((a311 a312 a313 a314)
      (a321 a322 a323 a324)
      (a331 a332 a333 a334)
      (a341 a342 a343 a344))
     ((a411 a412 a413 a414)
      (a421 a422 a423 a424)
      (a431 a432 a433 a434)
      (a441 a442 a443 a444)))
    (((b111 b112 b113 b114)
      (b121 b122 b123 b124)
      (b131 b132 b133 b134)
      (b141 b142 b143 b144))
     ((b211 b212 b213 b214)
      (b221 b222 b223 b224)
      (b231 b232 b233 b234)
      (b241 b242 b243 b244))
     ((b311 b312 b313 b314)
      (b321 b322 b323 b324)
      (b331 b332 b333 b334)
      (b341 b342 b343 b344))
     ((b411 b412 b413 b414)
      (b421 b422 b423 b424)
      (b431 b432 b433 b434)
      (b441 b442 b443 b444)))
    (((c111 c112 c113 c114)
      (c121 c122 c123 c124)
      (c131 c132 c133 c134)
      (c141 c142 c143 c144))
     ((c211 c212 c213 c214)
      (c221 c222 c223 c224)
      (c231 c232 c233 c234)
      (c241 c242 c243 c244))
     ((c311 c312 c313 c314)
      (c321 c322 c323 c324)
      (c331 c332 c333 c334)
      (c341 c342 c343 c344))
     ((c411 c412 c413 c414)
      (c421 c422 c423 c424)
      (c431 c432 c433 c434)
      (c441 c442 c443 c444)))
    (((d111 d112 d113 d114)
      (d121 d122 d123 d124)
      (d131 d132 d133 d134)
      (d141 d142 d143 d144))
     ((d211 d212 d213 d214)
      (d221 d222 d223 d224)
      (d231 d232 d233 d234)
      (d241 d242 d243 d244))
     ((d311 d312 d313 d314)
      (d321 d322 d323 d324)
      (d331 d332 d333 d334)
      (d341 d342 d343 d344))
     ((d411 d412 d413 d414)
      (d421 d422 d423 d424)
      (d431 d432 d433 d434)
      (d441 d442 d443 d444))))
) ;define

#|
caaar
连续执行三次 `car`。

语法
----
(caaar obj)

等价形式
--------
(car (car (car obj)))
|#
(check-true (procedure? caaar))
(check (caaar sample-cxr-3) => (car (car (car sample-cxr-3))))

#|
caadr
先执行一次 `cdr`，再连续执行两次 `car`。

语法
----
(caadr obj)

等价形式
--------
(car (car (cdr obj)))
|#
(check-true (procedure? caadr))
(check (caadr sample-cxr-3) => (car (car (cdr sample-cxr-3))))

#|
cadar
先执行 `car`，再执行 `cdr`，最后执行 `car`。

语法
----
(cadar obj)

等价形式
--------
(car (cdr (car obj)))
|#
(check-true (procedure? cadar))
(check (cadar sample-cxr-3) => (car (cdr (car sample-cxr-3))))

#|
caddr
先连续执行两次 `cdr`，再执行 `car`。

语法
----
(caddr obj)

等价形式
--------
(car (cdr (cdr obj)))
|#
(check-true (procedure? caddr))
(check (caddr sample-cxr-3) => (car (cdr (cdr sample-cxr-3))))

#|
cdaar
先连续执行两次 `car`，再执行 `cdr`。

语法
----
(cdaar obj)

等价形式
--------
(cdr (car (car obj)))
|#
(check-true (procedure? cdaar))
(check (cdaar sample-cxr-3) => (cdr (car (car sample-cxr-3))))

#|
cdadr
先执行一次 `cdr`，再执行 `car`，最后执行 `cdr`。

语法
----
(cdadr obj)

等价形式
--------
(cdr (car (cdr obj)))
|#
(check-true (procedure? cdadr))
(check (cdadr sample-cxr-3) => (cdr (car (cdr sample-cxr-3))))

#|
cddar
先执行一次 `car`，再连续执行两次 `cdr`。

语法
----
(cddar obj)

等价形式
--------
(cdr (cdr (car obj)))
|#
(check-true (procedure? cddar))
(check (cddar sample-cxr-3) => (cdr (cdr (car sample-cxr-3))))

#|
cdddr
连续执行三次 `cdr`。

语法
----
(cdddr obj)

等价形式
--------
(cdr (cdr (cdr obj)))
|#
(check-true (procedure? cdddr))
(check (cdddr sample-cxr-3) => (cdr (cdr (cdr sample-cxr-3))))

#|
caaaar
连续执行四次 `car`。

语法
----
(caaaar obj)

等价形式
--------
(car (car (car (car obj))))
|#
(check-true (procedure? caaaar))
(check (caaaar sample-cxr-4) => (car (car (car (car sample-cxr-4)))))

#|
caaadr
先执行一次 `cdr`，再连续执行三次 `car`。

语法
----
(caaadr obj)

等价形式
--------
(car (car (car (cdr obj))))
|#
(check-true (procedure? caaadr))
(check (caaadr sample-cxr-4) => (car (car (car (cdr sample-cxr-4)))))

#|
caadar
先执行 `car`，再执行 `cdr`，最后连续执行两次 `car`。

语法
----
(caadar obj)

等价形式
--------
(car (car (cdr (car obj))))
|#
(check-true (procedure? caadar))
(check (caadar sample-cxr-4) => (car (car (cdr (car sample-cxr-4)))))

#|
caaddr
先连续执行两次 `cdr`，再连续执行两次 `car`。

语法
----
(caaddr obj)

等价形式
--------
(car (car (cdr (cdr obj))))
|#
(check-true (procedure? caaddr))
(check (caaddr sample-cxr-4) => (car (car (cdr (cdr sample-cxr-4)))))

#|
cadaar
先连续执行两次 `car`，再执行 `cdr`，最后执行 `car`。

语法
----
(cadaar obj)

等价形式
--------
(car (cdr (car (car obj))))
|#
(check-true (procedure? cadaar))
(check (cadaar sample-cxr-4) => (car (cdr (car (car sample-cxr-4)))))

#|
cadadr
先执行一次 `cdr`，再执行 `car`，再执行 `cdr`，最后执行 `car`。

语法
----
(cadadr obj)

等价形式
--------
(car (cdr (car (cdr obj))))
|#
(check-true (procedure? cadadr))
(check (cadadr sample-cxr-4) => (car (cdr (car (cdr sample-cxr-4)))))

#|
caddar
先执行一次 `car`，再连续执行两次 `cdr`，最后执行 `car`。

语法
----
(caddar obj)

等价形式
--------
(car (cdr (cdr (car obj))))
|#
(check-true (procedure? caddar))
(check (caddar sample-cxr-4) => (car (cdr (cdr (car sample-cxr-4)))))

#|
cadddr
先连续执行三次 `cdr`，再执行 `car`。

语法
----
(cadddr obj)

等价形式
--------
(car (cdr (cdr (cdr obj))))
|#
(check-true (procedure? cadddr))
(check (cadddr sample-cxr-4) => (car (cdr (cdr (cdr sample-cxr-4)))))

#|
cdaaar
先连续执行三次 `car`，最后执行 `cdr`。

语法
----
(cdaaar obj)

等价形式
--------
(cdr (car (car (car obj))))
|#
(check-true (procedure? cdaaar))
(check (cdaaar sample-cxr-4) => (cdr (car (car (car sample-cxr-4)))))

#|
cdaadr
先执行一次 `cdr`，再连续执行两次 `car`，最后执行 `cdr`。

语法
----
(cdaadr obj)

等价形式
--------
(cdr (car (car (cdr obj))))
|#
(check-true (procedure? cdaadr))
(check (cdaadr sample-cxr-4) => (cdr (car (car (cdr sample-cxr-4)))))

#|
cdadar
先执行一次 `car`，再执行 `cdr`，再执行 `car`，最后执行 `cdr`。

语法
----
(cdadar obj)

等价形式
--------
(cdr (car (cdr (car obj))))
|#
(check-true (procedure? cdadar))
(check (cdadar sample-cxr-4) => (cdr (car (cdr (car sample-cxr-4)))))

#|
cdaddr
先连续执行两次 `cdr`，再执行 `car`，最后执行 `cdr`。

语法
----
(cdaddr obj)

等价形式
--------
(cdr (car (cdr (cdr obj))))
|#
(check-true (procedure? cdaddr))
(check (cdaddr sample-cxr-4) => (cdr (car (cdr (cdr sample-cxr-4)))))

#|
cddaar
先连续执行两次 `car`，再连续执行两次 `cdr`。

语法
----
(cddaar obj)

等价形式
--------
(cdr (cdr (car (car obj))))
|#
(check-true (procedure? cddaar))
(check (cddaar sample-cxr-4) => (cdr (cdr (car (car sample-cxr-4)))))

#|
cddadr
先执行一次 `cdr`，再执行 `car`，再连续执行两次 `cdr`。

语法
----
(cddadr obj)

等价形式
--------
(cdr (cdr (car (cdr obj))))
|#
(check-true (procedure? cddadr))
(check (cddadr sample-cxr-4) => (cdr (cdr (car (cdr sample-cxr-4)))))

#|
cdddar
先执行一次 `car`，再连续执行三次 `cdr`。

语法
----
(cdddar obj)

等价形式
--------
(cdr (cdr (cdr (car obj))))
|#
(check-true (procedure? cdddar))
(check (cdddar sample-cxr-4) => (cdr (cdr (cdr (car sample-cxr-4)))))

#|
cddddr
连续执行四次 `cdr`。

语法
----
(cddddr obj)

等价形式
--------
(cdr (cdr (cdr (cdr obj))))
|#
(check-true (procedure? cddddr))
(check (cddddr sample-cxr-4) => (cdr (cdr (cdr (cdr sample-cxr-4)))))

(check-report)
