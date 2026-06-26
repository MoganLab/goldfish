(define-library (liii base64)
  (import (scheme base) (liii base))
  (export string-base64-encode
    bytevector-base64-encode
    base64-encode
    string-base64-decode
    bytevector-base64-decode
    base64-decode
  ) ;export
  (begin
    (define string-base64-encode g_string-base64-encode)
    (define string-base64-decode g_string-base64-decode)
    (define bytevector-base64-encode g_bytevector-base64-encode)
    (define bytevector-base64-decode g_bytevector-base64-decode)
    (define base64-encode g_base64-encode)
    (define base64-decode g_base64-decode)
  ) ;begin
) ;define-library
