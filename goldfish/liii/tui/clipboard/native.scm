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

(define-library (liii tui clipboard native)
  (export native-clipboard-copy!)
  (import (scheme base) (scheme process-context) (liii os) (liii subprocess))
  (begin

    (define (native-clipboard-copy! text)
      (catch #t
        (lambda ()
          (cond ((os-windows?) (os-set-clipboard-text! text))
                ((os-macos?)
                 (zero? (run "pbcopy" :input text :stdout 'discard :stderr 'discard))
                ) ;
                ((os-linux?)
                 (or
                   (and (getenv "WAYLAND_DISPLAY")
                     (catch #t
                       (lambda ()
                         (zero? (run "wl-copy" :input text :stdout 'discard :stderr 'discard))
                       ) ;lambda
                       (lambda _ #f)
                     ) ;catch
                   ) ;and
                   (catch #t
                     (lambda ()
                       (zero? (run '("xclip" "-selection" "clipboard")
                                :input  text
                                :stdout 'discard
                                :stderr 'discard
                              ) ;run
                       ) ;zero?
                     ) ;lambda
                     (lambda _ #f)
                   ) ;catch
                   (catch #t
                     (lambda ()
                       (zero? (run '("xsel" "--clipboard" "--input")
                                :input  text
                                :stdout 'discard
                                :stderr 'discard
                              ) ;run
                       ) ;zero?
                     ) ;lambda
                     (lambda _ #f)
                   ) ;catch
                   #f
                 ) ;or
                ) ;
                (else #f)
          ) ;cond
        ) ;lambda
        (lambda _ #f)
      ) ;catch
    ) ;define

  ) ;begin
) ;define-library
