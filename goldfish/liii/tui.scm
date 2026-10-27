;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

(define-library (liii tui)
  (export make-component component? component-render component-handle-input
    component-invalidate! make-text make-truncated-text make-spacer strip-ansi
    display-width display-width-prefix truncate-to-width wrap-text parse-key
    parse-keys make-terminal-record terminal? terminal-size terminal-write
    terminal-cursor terminal-set-cursor! terminal-clear terminal-clear-line
    terminal-clear-line-to-end terminal-begin-sync! terminal-end-sync!
    terminal-cursor-visible? terminal-hide-cursor! terminal-show-cursor!
    terminal-flush terminal-detect-size make-virtual-terminal virtual-terminal?
    virtual-terminal-screen virtual-terminal-raw virtual-terminal-resize!
    virtual-terminal-push-key! virtual-terminal-pop-key! terminal-poll-key
    terminal-paste-mode? terminal-set-paste-mode! make-standard-terminal
    standard-terminal? terminal-enter-raw-mode! terminal-exit-raw-mode!
    terminal-max-y make-box box? make-vstack vstack? make-hstack hstack?
    make-container container? container-add! container-clear! container-children
    make-border-style border-style? border-style-resolve make-spinner spinner?
    make-loader loader? spinner-tick! spinner-reset! spinner-current-frame
    spinner-frame-index spinner-text spinner-set-text! spinner-prefix
    spinner-set-prefix! spinner-suffix spinner-set-suffix! spinner-interval
    spinner-set-interval! spinner-visible? spinner-set-visible! spinner-show!
    spinner-hide! spinner-status spinner-set-status! spinner-success!
    spinner-fail! spinner-error! spinner-warn! spinner-stop! spinner-presets
    spinner-frames-resolve make-editor editor? make-text-input text-input?
    make-text-area text-area? editor-text editor-set-text! editor-expanded-text
    editor-cursor editor-set-cursor! editor-display-col editor-lines
    editor-focused? editor-set-focused! editor-placeholder
    editor-set-placeholder! editor-history-list editor-history
    editor-set-on-submit! editor-border-color editor-set-border-color!
    make-history history? history-entries history-push! history-reset!
    make-main-screen main-screen? screen-terminal screen-buffer screen-lines
    screen-render! screen-refresh! screen-invalidate! screen-resize!
    screen-clear! screen-auto-hide-cursor? screen-auto-hide-cursor-set!
    make-screen-buffer screen-buffer? screen-buffer-lines
    screen-buffer-prev-lines screen-buffer-dirty? screen-diff screen-dirty-diff
    diff-op-type diff-op-index diff-op-text screen-diff-has-changes? fuzzy-match
    fuzzy-filter make-select-list select-list? select-list-items
    select-list-selected-item select-list-selected-index
    select-list-set-selected-index!
  ) ;export
  (import (liii tui component)
    (liii tui text-width)
    (liii tui keys)
    (liii tui terminal)
    (liii tui box)
    (liii tui loader)
    (liii tui editor)
    (liii tui main-screen)
    (liii tui fuzzy)
    (liii tui select-list)
  ) ;import
) ;define-library
