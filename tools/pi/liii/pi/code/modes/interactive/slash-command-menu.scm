;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 斜杠命令菜单：识别输入行首的命令上下文，过滤命令并构建选择列表
(define-library (liii pi code modes interactive slash-command-menu)
  (export pi-slash-command-matches make-pi-slash-command-menu)
  (import (scheme base) (liii string) (liii tui))
  (begin

    (define %accent "\x1b;[35m")
    (define %muted "\x1b;[38;2;157;165;169m")
    (define %reset "\x1b;[0m")

    (define %commands
      (list (list (cons 'name "model") (cons 'description "Select model"))
        (list (cons 'name "thinking") (cons 'description "Set thinking level"))
        (list (cons 'name "name") (cons 'description "Get or set session name"))
        (list (cons 'name "session") (cons 'description "Show session statistics"))
      ) ;list
    ) ;define

    (define (%colored color text)
      (string-append color text %reset)
    ) ;define

    ;; 仅在输入行（允许前导空白）以 / 开头且尚未输入命令参数时补全；
    ;; 无匹配时返回 #f，因此 /tmp 等路径不会留下空菜单。
    (define (pi-slash-command-matches text)
      (let ((command-text (string-trim-left text)))
        (if (and (string-starts? command-text "/")
              (not (string-contains? command-text " "))
            ) ;and
          (let ((matches
                  (fuzzy-filter %commands
                    (substring command-text 1 (string-length command-text))
                    (lambda (command) (cdr (assoc 'name command)))
                  ) ;fuzzy-filter
                ) ;matches
               ) ;
            (if (null? matches) #f matches)
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (make-pi-slash-command-menu commands)
      (make-select-list
        (map
          (lambda (command)
            (let ((name (cdr (assoc 'name command))))
              (list (cons 'value name)
                (cons 'label (string-append "/" name))
                (cons 'description (cdr (assoc 'description command)))
              ) ;list
            ) ;let
          ) ;lambda
          commands
        ) ;map
        5
        :min-primary-column-width 12
        :max-primary-column-width 32
        :selected-text            (lambda (text) (%colored %accent text))
        :description-text         (lambda (text) (%colored %muted text))
        :scroll-text              (lambda (text) (%colored %muted text))
        :no-match-text            (lambda (text) (%colored %muted text))
      ) ;make-select-list
    ) ;define

  ) ;begin
) ;define-library
