;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi template)
  (import (scheme base) (scheme process-context) (liii string) (liii error))
  (export pi-resolve-config-value lookup-env-var)
  (begin

    (define (env-name-start-char? ch)
      (or (and (char>=? ch #\a) (char<=? ch #\z))
        (and (char>=? ch #\A) (char<=? ch #\Z))
        (char=? ch #\_)
      ) ;or
    ) ;define

    (define (env-name-body-char? ch)
      (or (env-name-start-char? ch) (and (char>=? ch #\0) (char<=? ch #\9)))
    ) ;define

    (define (valid-env-name? str)
      (let ((len (string-length str)))
        (and (> len 0)
          (env-name-start-char? (string-ref str 0))
          (string-every env-name-body-char? (substring str 1))
        ) ;and
      ) ;let
    ) ;define

    (define (lookup-env-var name custom-env)
      (if (pair? custom-env)
        (let ((pair (assoc name (car custom-env))))
          (if pair (cdr pair) #f)
        ) ;let
        (get-environment-variable name)
      ) ;if
    ) ;define

    (define (extract-env-var-name str start len)
      (let loop
        ((i start))
        (cond
         ((and (< i len) (env-name-body-char? (string-ref str i))) (loop (+ i 1)))
         (else (values (substring str start i) i))
        ) ;cond
      ) ;let
    ) ;define

    (define (resolve-braced-var str i len custom-env parts)
      (let ((brace-end (string-index str #\} (+ i 2))))
        (if (not brace-end)
          (values (+ i 1) (cons "$" parts))
          (let ((name (substring str (+ i 2) brace-end)))
            (if (valid-env-name? name)
              (let ((val (lookup-env-var name custom-env)))
                (if (not val) (values -1 #f) (values (+ brace-end 1) (cons val parts)))
              ) ;let
              (values (+ brace-end 1) (cons (substring str i (+ brace-end 1)) parts))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (resolve-simple-var str i len custom-env parts)
      (let-values (((name next-idx) (extract-env-var-name str (+ i 1) len)))
        (let ((val (lookup-env-var name custom-env)))
          (if (not val) (values -1 #f) (values next-idx (cons val parts)))
        ) ;let
      ) ;let-values
    ) ;define

    (define (process-dollar-token str i len custom-env parts)
      (let ((c1 (string-ref str (+ i 1))))
        (cond
         ((or (char=? c1 #\$) (char=? c1 #\!)) (values (+ i 2) (cons (string c1) parts)))
         ((char=? c1 #\{) (resolve-braced-var str i len custom-env parts))
         ((env-name-start-char? c1) (resolve-simple-var str i len custom-env parts))
         (else (values (+ i 1) (cons "$" parts)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%advance-dollar str dollar-pos len custom-env parts-with-prefix)
      (if (>= (+ dollar-pos 1) len)
        (values -2 (cons "$" parts-with-prefix))
        (process-dollar-token str dollar-pos len custom-env parts-with-prefix)
      ) ;if
    ) ;define

    (define (%loop-template-parts str len custom-env)
      (let loop
        ((i 0) (parts '()))
        (cond ((>= i len) (apply string-append (reverse parts)))
              (else
                (let ((dollar-pos (string-index str #\$ i)))
                  (if (not dollar-pos)
                    (apply string-append (reverse (cons (substring str i len) parts)))
                    (let* ((prefix (substring str i dollar-pos))
                           (parts1 (if (> (string-length prefix) 0) (cons prefix parts) parts))
                          ) ;
                      (let-values (((next-i next-parts) (%advance-dollar str dollar-pos len custom-env parts1)))
                        (cond ((= next-i -1) #f)
                              ((= next-i -2) (apply string-append (reverse next-parts)))
                              (else (loop next-i next-parts))
                        ) ;cond
                      ) ;let-values
                    ) ;let*
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-resolve-config-value str . custom-env)
      (when (not (string? str))
        (type-error "pi-resolve-config-value: expected string")
      ) ;when
      (if (string-starts? str "!")
        (error 'pi-config-error "command syntax not supported in config value" str)
        (%loop-template-parts str (string-length str) custom-env)
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
