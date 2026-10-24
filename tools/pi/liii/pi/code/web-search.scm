;;
;; Ported from pi-web-search (https://github.com/ttttmr/pi-web-search)
;; Original work Copyright (c) 2026 ttttmr
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code web-search)
  (import (scheme base)
    (liii pi code web-search schema)
    (liii pi code web-search format)
    (liii pi code web-search execute)
  ) ;import
  (export pi-web-search-schema pi-validate-web-search-args
    pi-format-web-search-result pi-execute-web-search
  ) ;export
  (begin
  ) ;begin
) ;define-library
