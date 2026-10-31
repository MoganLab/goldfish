;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 配置文件路径与读取（相对配置目录；展示与写入都用相对路径）
(define-library (liii pi code modes config paths)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii string)
    (liii path)
    (liii pi code modes config state)
  ) ;import
  (export pi-config-rel-provider pi-config-rel-model pi-config-abs
    pi-config-read-text
  ) ;export
  (begin

    (define (pi-config-rel-provider p-id)
      (string-append "models/" p-id ".json")
    ) ;define

    (define (pi-config-rel-model p-id m-id)
      (string-append "models/" p-id "/" m-id ".json")
    ) ;define

    (define (pi-config-abs st rel)
      (string-append (pi-config-state-dir st) "/" rel)
    ) ;define

    ;; 读原文；文件缺失或读取失败返回 #f
    (define (pi-config-read-text st rel)
      (let ((abs (pi-config-abs st rel)))
        (and (file-exists? abs)
          (catch #t (lambda () (path-read-text (path abs))) (lambda args #f))
        ) ;and
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
