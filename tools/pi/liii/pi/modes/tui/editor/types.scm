;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui editor types)
  (import (scheme base) (liii base) (liii error) (liii tui editor))
  (export <pi-tui-editor-data> %make-pi-tui-editor-data pi-tui-editor-data?
    ed-data-buffer ed-data-history ed-data-prompt ed-data-prompt-set!
    ed-data-continuation-prompt ed-data-continuation-prompt-set!
    ed-data-placeholder ed-data-placeholder-set! ed-data-focused?
    ed-data-focused?-set! ed-data-cursor-style ed-data-cursor-style-set!
    ed-data-on-submit ed-data-on-submit-set! ed-data-on-change
    ed-data-on-change-set! ed-data-border-style ed-data-border-style-set!
  ) ;export
  (begin

    (define-record-type <pi-tui-editor-data>
      (%make-pi-tui-editor-data buffer history prompt continuation-prompt
        placeholder focused? cursor-style on-submit on-change border-style
      ) ;%make-pi-tui-editor-data
      pi-tui-editor-data?
      (buffer ed-data-buffer)
      (history ed-data-history)
      (prompt ed-data-prompt ed-data-prompt-set!)
      (continuation-prompt ed-data-continuation-prompt
        ed-data-continuation-prompt-set!
      ) ;continuation-prompt
      (placeholder ed-data-placeholder ed-data-placeholder-set!)
      (focused? ed-data-focused? ed-data-focused?-set!)
      (cursor-style ed-data-cursor-style ed-data-cursor-style-set!)
      (on-submit ed-data-on-submit ed-data-on-submit-set!)
      (on-change ed-data-on-change ed-data-on-change-set!)
      (border-style ed-data-border-style ed-data-border-style-set!)
    ) ;define-record-type

    (define <pi-tui-editor-data> '<pi-tui-editor-data>)

  ) ;begin
) ;define-library
