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

(define-library (liii tui terminal)
  (export make-terminal-record terminal? terminal-size terminal-write
    terminal-cursor terminal-set-cursor! terminal-clear terminal-clear-line
    terminal-clear-line-to-end terminal-begin-sync! terminal-end-sync!
    terminal-cursor-visible? terminal-hide-cursor! terminal-show-cursor!
    terminal-flush terminal-detect-size make-virtual-terminal virtual-terminal?
    virtual-terminal-screen virtual-terminal-raw virtual-terminal-resize!
    virtual-terminal-push-key! virtual-terminal-pop-key! terminal-poll-key
    terminal-paste-mode? terminal-set-paste-mode! make-standard-terminal
    standard-terminal? terminal-enter-raw-mode! terminal-exit-raw-mode!
    terminal-max-y terminal-mouse-mode!
  ) ;export
  (import (liii tui terminal protocol)
    (liii tui terminal size)
    (liii tui terminal virtual)
    (liii tui terminal standard)
    (liii tui terminal poll)
  ) ;import
) ;define-library
