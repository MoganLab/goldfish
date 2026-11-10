;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core skills types)
  (export %make-pi-skill pi-skill? pi-skill-name pi-skill-description
    pi-skill-file-path pi-skill-base-dir pi-skill-disable-model-invocation?
    pi-validate-skill-name pi-validate-skill-description
  ) ;export
  (import (scheme base) (scheme char) (liii string) (liii list))
  (begin

    (define %max-name-length 64)
    (define %max-description-length 1024)

    (define-record-type <pi-skill>
      (%make-pi-skill name description file-path base-dir disable-model-invocation?)
      pi-skill?
      (name pi-skill-name)
      (description pi-skill-description)
      (file-path pi-skill-file-path)
      (base-dir pi-skill-base-dir)
      (disable-model-invocation? pi-skill-disable-model-invocation?)
    ) ;define-record-type

    ;; 验证技能名称是否符合 Agent Skills 规范
    ;; 仅允许小写 a-z、数字 0-9、短横线 -；长度在 1 到 64 字符；
    ;; 禁止以 - 起始或结尾；禁止含连续双短横线 --
    (define (pi-validate-skill-name name)
      (if (not (string? name))
        (list "name must be a string")
        (let ((len (string-length name)))
          (filter (lambda (x) x)
            (list (and (> len %max-name-length)
                    (string-append "name exceeds "
                      (number->string %max-name-length)
                      " characters ("
                      (number->string len)
                      ")"
                    ) ;string-append
                  ) ;and
              (and (= len 0) "name is required")
              ;; 校验字符集：^[a-z0-9-]+$
              (and (> len 0)
                (not
                  (string-every (lambda (c) (or (char<=? #\a c #\z) (char<=? #\0 c #\9) (char=? c #\-)))
                    name
                  ) ;string-every
                ) ;not
                "name contains invalid characters (must be lowercase a-z, 0-9, hyphens only)"
              ) ;and
              (and (> len 0)
                (or (char=? (string-ref name 0) #\-) (char=? (string-ref name (- len 1)) #\-))
                "name must not start or end with a hyphen"
              ) ;and
              (and (string-contains? name "--") "name must not contain consecutive hyphens")
            ) ;list
          ) ;filter
        ) ;let
      ) ;if
    ) ;define

    ;; 验证技能描述是否符合 Agent Skills 规范
    ;; 必填非空字符串，长度不超过 1024 字符
    (define (pi-validate-skill-description description)
      (cond
       ((or (not (string? description)) (string-null? (string-trim-both description)))
        (list "description is required")
       ) ;
       ((> (string-length description) %max-description-length)
        (list (string-append "description exceeds "
                (number->string %max-description-length)
                " characters ("
                (number->string (string-length description))
                ")"
              ) ;string-append
        ) ;list
       ) ;
       (else '())
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
