;;; liii/bigint/conversion.scm - bigint 构造与转换
;;;
;;; 负责 bigint 与字符串、整数、字节向量之间的转换。
;;;
;;; Copyright (c) 2024-2026 Liii Network
;;; All Rights Reserved

(define-library (liii bigint conversion)
  (import (scheme base) (liii bigint string) (liii bigint bytes))

  (export string->bigint integer->bigint bigint->string bytes->bigint
    bigint->bytes
  ) ;export

  (begin
  ) ;begin
) ;define-library
