;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi tui)
  (export make-pi-header pi-header? make-pi-footer pi-footer? make-pi-app
    pi-app? pi-app-screen pi-app-editor pi-app-render! pi-app-run pi-app-queue
    pi-app-chat pi-app-submit! pi-app-drain-queue! pi-app-add-user-message!
    pi-app-add-tool-result! pi-app-add-assistant-text! pi-app-event-chan
    pi-app-poll-events! make-user-card make-tool-card make-assistant-card
    make-queued-card apply-card-bg make-tui-agent-handler
  ) ;export
  (import (liii pi tui header)
    (liii pi tui footer)
    (liii pi tui app)
    (liii pi tui views)
    (liii pi tui handler)
  ) ;import
) ;define-library
