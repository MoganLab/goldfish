;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive views skill-card)
  (export make-skill-card skill-card? skill-card-component? skill-card-expanded?
    skill-card-set-expanded!
  ) ;export
  (import (scheme base)
    (liii string)
    (liii tui component)
    (liii tui mouse)
    (liii pi code modes interactive views cache)
    (liii pi code modes interactive views theme)
    (liii pi code modes interactive views wrap)
  ) ;import
  (begin

    ;; 技能折叠卡片状态（对标上游 SkillInvocationMessageComponent）
    (define-record-type <skill-card>
      (%make-skill-card name content expanded? render-cache)
      skill-card?
      (name %skill-card-name)
      (content %skill-card-content)
      (expanded? %skill-card-expanded? %skill-card-expanded?-set!)
      (render-cache %skill-card-render-cache)
    ) ;define-record-type

    (define (skill-card-set-expanded! card flag)
      (%skill-card-expanded?-set! (component-data card) (and flag #t))
    ) ;define

    (define (skill-card-expanded? card)
      (%skill-card-expanded? (component-data card))
    ) ;define

    (define (skill-card-component? obj)
      (and (component? obj) (skill-card? (component-data obj)))
    ) ;define

    (define (%render-skill-card st width)
      (%cached-render (%skill-card-render-cache st)
        (list (%skill-card-expanded? st) width)
        (lambda ()
          (if (<= width 0)
            '()
            (if (%skill-card-expanded? st)
              ;; 展开状态：顶底 padding、标题段与正文 Markdown
              (let* ((title-line (string-append " [skill] **" (%skill-card-name st) "**"))
                     (body-rows
                       (%card-body-lines (%skill-card-content st)
                         width
                         1
                         1
                         (lambda (line)
                           (list (apply-card-bg (string-append " " line) width %bg-user %fg-user))
                         ) ;lambda
                       ) ;%card-body-lines
                     ) ;body-rows
                    ) ;
                (append (list (apply-card-bg "" width %bg-user %fg-user)
                          (apply-card-bg title-line width %bg-user %fg-user)
                        ) ;list
                  body-rows
                  (list (apply-card-bg "" width %bg-user %fg-user) "")
                ) ;append
              ) ;let*
              ;; 折叠状态：紧凑单行卡片 [skill] <name> (Ctrl+O 展开)
              (let ((line (string-append " [skill] " (%skill-card-name st) " (Ctrl+O 展开)")))
                (list (apply-card-bg line width %bg-user %fg-user) "")
              ) ;let
            ) ;if
          ) ;if
        ) ;lambda
      ) ;%cached-render
    ) ;define

    (define* (make-skill-card name content (expanded? #f))
      (let* ((st (%make-skill-card name content (and expanded? #t) (%make-render-cache #f '()))
             ) ;st
             (card
               (make-component
                 :render (lambda (self width) (%render-skill-card st width))
                 ;; 鼠标点击切换折叠/展开（对齐上游 MouseRegion click 左键）
                 :handle-mouse
                 (lambda (self ev)
                   (if (and (mouse-event-click? ev) (eq? (mouse-event-button ev) 'left))
                     (begin
                       (%skill-card-expanded?-set! st (not (%skill-card-expanded? st)))
                       #t
                     ) ;begin
                     #f
                   ) ;if
                 ) ;lambda
               ) ;make-component
             ) ;card
            ) ;
        (component-data-set! card st)
        card
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
