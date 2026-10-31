;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `gf pi config` 的写盘层：
;;   pi-config-json-pretty  2 空格缩进、保序的 JSON 序列化
;;   pi-config-apply-writes 顺序原子写（.tmp -> 替换原文件），失败即停
(define-library (liii pi code modes config json-write)
  (import (scheme base)
    (liii base)
    (liii list)
    (liii string)
    (liii unicode)
    (liii path)
    (liii json)
    (liii pi code modes config state)
  ) ;import
  (export pi-config-json-pretty pi-config-apply-writes)
  (begin

    ;; ---------- JSON 序列化（2 空格缩进，保序） ----------

    (define (%pad n)
      (utf8-make-string n #\space)
    ) ;define

    (define (%scalar? v)
      (or (string? v) (number? v) (boolean? v))
    ) ;define

    ;; 字符串转义：保留 "/" 原样（json->string 会转义成 \/，配置文件里很难读）
    (define (%escape-string str)
      (let* ((len (string-length str)) (out (open-output-string)))
        (write-char #\" out)
        (let loop
          ((i 0))
          (when (< i len)
            (let ((ch (string-ref str i)))
              (cond ((char=? ch #\") (display "\\\"" out))
                    ((char=? ch #\\) (display "\\\\" out))
                    ((char=? ch #\newline) (display "\\n" out))
                    ((char=? ch #\return) (display "\\r" out))
                    ((char=? ch #\tab) (display "\\t" out))
                    (else (write-char ch out))
              ) ;cond
            ) ;let
            (loop (+ i 1))
          ) ;when
        ) ;let
        (write-char #\" out)
        (get-output-string out)
      ) ;let*
    ) ;define
    (define (%scalar->string v)
      (cond ((boolean? v) (if v "true" "false"))
            ((string? v) (%escape-string v))
            (else (json->string v))
      ) ;cond
    ) ;define
    (define (%indent-of level)
      (%pad (* 2 level))
    ) ;define
    (define (%inline-array? v)
      (and (vector? v) (every %scalar? (vector->list v)))
    ) ;define
    (define (%inline-array->string v)
      (string-append "["
        (string-join (map %scalar->string (vector->list v)) ", ")
        "]"
      ) ;string-append
    ) ;define
    (define (%json-array->lines v level)
      (if (null? (vector->list v))
        (list "[]")
        (append (list "[")
          (let loop
            ((rest (vector->list v)) (acc '()) (first? #t))
            (if (null? rest)
              (reverse acc)
              (loop (cdr rest)
                (cons
                  (string-append (%indent-of (+ level 1))
                    (pi-config-json-pretty (car rest) (+ level 1))
                    (if (null? (cdr rest)) "" ",")
                  ) ;string-append
                  acc
                ) ;cons
                #f
              ) ;loop
            ) ;if
          ) ;let
          (list (string-append (%indent-of level) "]"))
        ) ;append
      ) ;if
    ) ;define
    (define (%json-object->lines alist level)
      (if (null? alist)
        (list "{}")
        (append (list "{")
          (let loop
            ((rest alist) (acc '()))
            (if (null? rest)
              (reverse acc)
              (let* ((entry (car rest))
                     (key (%scalar->string (car entry)))
                     (val (cdr entry))
                     (sep (if (null? (cdr rest)) "" ","))
                     (body (pi-config-json-pretty val (+ level 1)))
                    ) ;
                (loop (cdr rest)
                  (cons (string-append (%indent-of (+ level 1)) key ": " body sep) acc)
                ) ;loop
              ) ;let*
            ) ;if
          ) ;let
          (list (string-append (%indent-of level) "}"))
        ) ;append
      ) ;if
    ) ;define
    (define* (pi-config-json-pretty v (level 0))
      (cond ((%scalar? v) (%scalar->string v))
            ((and (vector? v) (%inline-array? v)) (%inline-array->string v))
            ((vector? v) (string-join (%json-array->lines v level) "\n"))
            ((pair? v) (string-join (%json-object->lines v level) "\n"))
            ((null? v) "{}")
            (else (%scalar->string v))
      ) ;cond
    ) ;define*
    (define (%write-atomic abs text)
      (let* ((tmp (string-append abs ".tmp")) (bak (string-append abs ".bak")))
        (path-write-text (path tmp) text)
        (if (file-exists? abs)
          (begin
            (path-unlink (path bak) :missing-ok #t)
            (path-rename (path abs) (path bak))
            (path-rename (path tmp) (path abs))
            (path-unlink (path bak) :missing-ok #t)
          ) ;begin
          (path-rename (path tmp) (path abs))
        ) ;if
      ) ;let*
    ) ;define
    (define (%abs st rel)
      (string-append (pi-config-state-dir st) "/" rel)
    ) ;define
    (define (%write-atomic abs text)
      (let* ((tmp (string-append abs ".tmp")) (bak (string-append abs ".bak")))
        (path-write-text (path tmp) text)
        (if (file-exists? abs)
          (begin
            (path-unlink (path bak) :missing-ok #t)
            (path-rename (path abs) (path bak))
            (path-rename (path tmp) (path abs))
            (path-unlink (path bak) :missing-ok #t)
          ) ;begin
          (path-rename (path tmp) (path abs))
        ) ;if
      ) ;let*
    ) ;define
    (define (pi-config-apply-writes st writes)
      (let loop
        ((rest writes) (done '()))
        (if (null? rest)
          (list (cons "ok" #t)
            (cons "message" (string-append "saved " (string-join (reverse done) ", ")))
            (cons "failed" #f)
          ) ;list
          (let* ((entry (car rest)) (rel (car entry)) (abs (%abs st rel)))
            (catch #t
              (lambda ()
                (let ((dir (path-parent (path abs))))
                  (unless (path-exists? dir)
                    (path-mkdir dir :parents #t :exist-ok #t)
                  ) ;unless
                ) ;let
                (%write-atomic abs (cdr entry))
                (loop (cdr rest) (cons rel done))
              ) ;lambda
              (lambda (tag . info)
                (list (cons "ok" #f)
                  (cons "message"
                    (string-append "save failed: "
                      rel
                      " · "
                      (if (pair? info) (object->string (car info)) "")
                    ) ;string-append
                  ) ;cons
                  (cons "failed" rel)
                ) ;list
              ) ;lambda
            ) ;catch
          ) ;let*
        ) ;if
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
