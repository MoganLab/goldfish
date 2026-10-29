;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive)
  (export make-pi-header pi-header? make-pi-footer pi-footer?
    pi-footer-model-label pi-footer-set-thinking-level! pi-footer-branch
    pi-footer-set-branch! make-pi-app pi-app? pi-app-screen pi-app-editor
    pi-app-render! pi-app-run pi-app-queue pi-app-chat pi-app-submit!
    pi-app-submit-initial! pi-app-drain-queue! pi-app-add-user-message!
    pi-app-add-tool-result! pi-app-add-assistant-text! pi-app-add-retry-message!
    pi-app-jobs-chan pi-app-event-chan pi-app-poll-events! pi-app-thinking-level
    pi-app-supports-thinking? pi-app-cycle-thinking! pi-next-thinking-level
    pi-app-toggle-tool-output! pi-app-active-selector
    pi-app-default-thinking-level pi-thinking-border-color make-user-card
    make-tool-card make-assistant-card make-queued-card make-retry-card
    status-card? apply-card-bg make-tui-agent-handler pi-app-scroll-view
    pi-app-scroll-up! pi-app-scroll-down! pi-app-scroll-top!
    pi-app-scroll-bottom! pi-app-scroll-line-up! pi-app-scroll-line-down!
    pi-app-frame-lines
  ) ;export
  (import (liii pi code modes interactive header)
    (liii pi code modes interactive footer)
    (liii pi code modes interactive thinking)
    (liii pi code modes interactive app)
    (liii pi code modes interactive run)
    (liii pi code modes interactive views)
    (liii pi code modes interactive status-card)
    (liii pi code modes interactive handler)
  ) ;import
) ;define-library
