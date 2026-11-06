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

(define-library (liii tui loader)
  (export spinner-presets spinner-frames-resolve spinner-status-icon
    make-spinner spinner? make-loader loader? spinner-frames spinner-frame-index
    spinner-current-frame spinner-text spinner-set-text! spinner-prefix
    spinner-set-prefix! spinner-suffix spinner-set-suffix! spinner-interval
    spinner-set-interval! spinner-visible? spinner-set-visible! spinner-show!
    spinner-hide! spinner-status spinner-set-status! spinner-success!
    spinner-fail! spinner-error! spinner-warn! spinner-stop! spinner-tick!
    spinner-reset!
  ) ;export
  (import (liii tui loader presets) (liii tui loader spinner))
) ;define-library
