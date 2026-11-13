;;; liii/decimal/conversion.scm - decimal 构造与格式化
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define-library (liii decimal conversion)
  (import (scheme base) (liii decimal string) (liii decimal type))

  (export string->decimal decimal->string decimal->plain-string
    decimal->engineering-string decimal->scientific-string decimal->long
    decimal->long-exact decimal->int decimal->int-exact decimal->short
    decimal->short-exact decimal->byte decimal->byte-exact decimal->double
    decimal->float decimal->rational
  ) ;export

  (begin
  ) ;begin
) ;define-library
