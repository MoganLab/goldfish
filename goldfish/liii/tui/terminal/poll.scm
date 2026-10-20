;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2024-2026 The Goldfish Scheme Authors
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

(define-library (liii tui terminal poll)
  (export terminal-poll-key %poll-console-key)
  (import (scheme base)
    (liii error)
    (liii os)
    (liii tui terminal protocol)
    (liii tui terminal virtual)
  ) ;import
  (begin

    (define (%map-extended-key code)
      (cond ((= code 72) 'up)
            ((= code 80) 'down)
            ((= code 75) 'left)
            ((= code 77) 'right)
            ((= code 71) 'home)
            ((= code 79) 'end)
            ((= code 73) 'pageup)
            ((= code 81) 'pagedown)
            ((= code 83) 'delete)
            (else #f)
      ) ;cond
    ) ;define

    (define (%map-standard-key code)
      (cond ((or (= code 13) (= code 10)) 'enter)
            ((= code 27) 'escape)
            ((or (= code 8) (= code 127)) 'backspace)
            ((= code 9) 'tab)
            ((= code 3) 'ctrl-c)
            ((= code 4) 'ctrl-d)
            ((and (>= code 32) (<= code 126)) (string (integer->char code)))
            (else #f)
      ) ;cond
    ) ;define

    (define (%poll-console-key)
      (if (console-kbhit?)
        (let ((c (console-getch)))
          (if (or (= c 0) (= c 224))
            (let ((ext (console-getch)))
              (%map-extended-key ext)
            ) ;let
            (%map-standard-key c)
          ) ;if
        ) ;let
        #f
      ) ;if
    ) ;define

    (define (terminal-poll-key . args)
      (let ((term (if (null? args) #f (car args))))
        (cond ((not term) (%poll-console-key))
              ((virtual-terminal? term) (virtual-terminal-pop-key! term))
              ((terminal? term) (%poll-console-key))
              (else (type-error "terminal-poll-key: expected a terminal or #f"))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
