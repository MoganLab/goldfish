;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi tui)
  (export make-pi-header pi-header? make-pi-footer pi-footer? make-pi-app
    pi-app? pi-app-screen pi-app-editor pi-app-render! pi-app-run
  ) ;export
  (import (liii pi tui header) (liii pi tui footer) (liii pi tui app))
) ;define-library
