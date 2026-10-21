;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi tui)
  (export make-pi-header pi-header? make-pi-footer pi-footer? make-pi-app
    pi-app? pi-app-screen pi-app-editor pi-app-render! pi-app-run make-user-card
    make-tool-card make-assistant-card make-queued-card apply-card-bg
  ) ;export
  (import (liii pi tui header)
    (liii pi tui footer)
    (liii pi tui app)
    (liii pi tui views)
  ) ;import
) ;define-library
