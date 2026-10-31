;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 用户配置目录（~/.gf/agent）读取的公共小工具：
;; JSON 读取、alist 取键（字符串/符号键均可）、告警输出、向量转列表。
(define-library (liii pi code config user-fs)
  (import (scheme base) (scheme file) (liii string) (liii path) (liii json))
  (export pi-user-dict-ref pi-user-read-json pi-user-warn pi-user-vector->list
    pi-user-config-path
  ) ;export
  (begin

    (define (pi-user-dict-ref obj key default-val)
      (if (not (pair? obj))
        default-val
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (or (assoc str-key obj) (assoc sym-key obj)))
              ) ;
          (if pair (cdr pair) default-val)
        ) ;let*
      ) ;if
    ) ;define

    ;; 读取 JSON 文件；缺失或语法错误返回 #f
    (define (pi-user-read-json filepath)
      (and (file-exists? filepath)
        (catch #t
          (lambda () (string->json (path-read-text (path filepath))))
          (lambda args #f)
        ) ;catch
      ) ;and
    ) ;define

    (define (pi-user-warn msg)
      (display "[warn] ")
      (display msg)
      (newline)
    ) ;define

    (define (pi-user-vector->list v)
      (if (vector? v) (vector->list v) v)
    ) ;define

    ;; 配置目录下的路径拼接（返回字符串）
    (define (pi-user-config-path config-dir . segments)
      (path->string (apply path-join (cons (path config-dir) (map path segments))))
    ) ;define

  ) ;begin
) ;define-library
