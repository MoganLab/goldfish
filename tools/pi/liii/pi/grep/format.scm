;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; grep 结果的格式化：匹配块、结果前缀与提示（notice）。
;; 单独成模块以遵守 300 行/文件的硬约束（见 gf_check.json）。

(define-library (liii pi grep format)
  (import (scheme base) (liii string) (liii pi truncate))
  (export %format-grep-block %format-grep-matches %build-grep-notices
    %skipped-files-notice %grep-empty-result
  ) ;export
  (begin

    (define (%format-grep-block file-lines rel-path line-num context)
      (let* ((total (length file-lines))
             (start (if (> context 0) (max 1 (- line-num context)) line-num))
             (end (if (> context 0) (min total (+ line-num context)) line-num))
            ) ;
        (let loop
          ((curr start) (acc '()) (any-trunc? #f))
          (if (> curr end)
            (values (reverse acc) any-trunc?)
            (let* ((line (list-ref file-lines (- curr 1)))
                   (is-match? (= curr line-num))
                   (sep (if is-match? ":" "-"))
                  ) ;
              (let-values (((trunc-text was-trunc?) (pi-truncate-line line)))
                (let ((entry (string-append rel-path sep (number->string curr) sep " " trunc-text)))
                  (loop (+ curr 1) (cons entry acc) (or any-trunc? was-trunc?))
                ) ;let
              ) ;let-values
            ) ;let*
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    ;; 有文件读不出来时明确告知：静默跳过会让「无匹配」掩盖真实失败
    (define (%skipped-files-notice skipped)
      (string-append (number->string (length skipped))
        " files skipped (unreadable). First: "
        (car skipped)
      ) ;string-append
    ) ;define

    ;; 没有匹配结果时，期间有文件读不出来就必须说明，不能谎报 No matches found
    (define (%grep-empty-result skipped)
      (if (null? skipped) "No matches found" (%skipped-files-notice skipped))
    ) ;define

    (define (%build-grep-notices limit-reached? limit truncated? lines-truncated? skipped)
      (let* ((n1
               (if limit-reached?
                 (list (string-append (number->string limit)
                         " matches limit reached. Use limit="
                         (number->string (* limit 2))
                         " for more, or refine pattern"
                       ) ;string-append
                 ) ;list
                 '()
               ) ;if
             ) ;n1
             (n2 (if truncated? (append n1 (list "50.0KB limit reached")) n1))
             (n3
               (if (null? skipped) n2 (append n2 (list (%skipped-files-notice skipped))))
             ) ;n3
             (notices (if lines-truncated?
                        (append n3
                          (list "Some lines truncated to 500 chars. Use read tool to see full lines")
                        ) ;append
                        n3
                      ) ;if
             ) ;notices
            ) ;
        (if (null? notices) "" (string-append "\n\n[" (string-join notices ". ") "]"))
      ) ;let*
    ) ;define

    (define (%format-grep-matches all-matches cache ctx-val limit limit-reached? skipped)
      (let block-loop
        ((m-list all-matches) (out-lines '()) (any-line-trunc? #f))
        (if (null? m-list)
          (let* ((raw-out (string-join (reverse out-lines) "\n"))
                 (trunc (pi-truncate-head raw-out 999999 PI_DEFAULT_MAX_BYTES))
                ) ;
            (string-append (pi-trunc-result-content trunc)
              (%build-grep-notices limit-reached?
                limit
                (pi-trunc-result-truncated trunc)
                any-line-trunc?
                skipped
              ) ;%build-grep-notices
            ) ;string-append
          ) ;let*
          (let* ((m (car m-list))
                 (rel-f (car m))
                 (line-n (cadr m))
                 (lines (cdr (assoc rel-f cache)))
                ) ;
            (let-values (((block trunc?) (%format-grep-block lines rel-f line-n ctx-val)))
              (block-loop (cdr m-list)
                (append (reverse block) out-lines)
                (or any-line-trunc? trunc?)
              ) ;block-loop
            ) ;let-values
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
