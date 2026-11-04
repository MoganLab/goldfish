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
  (export native-clipboard-copy! native-clipboard-text)
  (import (scheme base)
    (scheme process-context)
    (liii os)
    (liii string)
    (liii either)
    (liii subprocess)
  ) ;import
  (begin

    (define (%clipboard-command-output command)
      (catch #t
        (lambda ()
          (let ((result (run-either command :timeout 5 :stderr 'discard)))
            (if (either-right? result) (to-right result) #f)
          ) ;let
        ) ;lambda
        (lambda _ #f)
      ) ;catch
    ) ;define

    (define (%first-clipboard-text commands runner)
      (cond ((null? commands) #f)
            (else
              (let ((text (runner (car commands))))
                (if (and (string? text) (not (string-null? text)))
                  text
                  (%first-clipboard-text (cdr commands) runner)
                ) ;if
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; 测试可注入 runner：参数为 argv 列表，返回文本或 #f。
    (define* (native-clipboard-text (runner %clipboard-command-output))
      (catch #t
        (lambda ()
          (if (os-linux?)
            (%first-clipboard-text
              (append (if (getenv "WAYLAND_DISPLAY")
                        '(("wl-paste" "--no-newline" "--type" "text"))
                        '()
                      ) ;if
                (if (getenv "DISPLAY")
                  '(("xclip" "-selection" "clipboard" "-out")
                    ("xsel" "--clipboard" "--output"))
                  '()
                ) ;if
              ) ;append
              runner
            ) ;%first-clipboard-text
            #f
          ) ;if
        ) ;lambda
        (lambda _ #f)
      ) ;catch
    ) ;define*

    (define (native-clipboard-copy! text)
      (catch #t
        (lambda ()
          (cond ((os-windows?) (g_os_set_clipboard_text text))
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
