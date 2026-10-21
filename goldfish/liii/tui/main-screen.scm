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

(define-library (liii tui main-screen)
  (export <main-screen> make-main-screen main-screen? screen-terminal
    screen-buffer screen-lines screen-render! screen-refresh! screen-invalidate!
    screen-resize! screen-clear! screen-auto-hide-cursor?
    screen-auto-hide-cursor-set! <screen-buffer> make-screen-buffer
    screen-buffer? screen-buffer-lines screen-buffer-lines-set!
    screen-buffer-prev-lines screen-buffer-prev-lines-set! screen-buffer-dirty?
    screen-buffer-dirty-set! screen-buffer-invalidate! screen-buffer-commit!
    screen-buffer-clear! screen-diff screen-dirty-diff diff-op-type
    diff-op-index diff-op-text screen-diff-has-changes?
  ) ;export
  (import (liii tui main-screen diff)
    (liii tui main-screen buffer)
    (liii tui main-screen driver)
  ) ;import
) ;define-library
