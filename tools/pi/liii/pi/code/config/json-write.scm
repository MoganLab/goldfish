;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 用户配置目录的 JSON 序列化与写盘层：
;;   pi-config-json-pretty  多行、2 空格缩进的 JSON 序列化（json->string :indent 2）
;;   pi-write-atomic        原子写单个文件（.tmp -> .bak -> 原文件）
;;   pi-apply-writes        顺序原子写（失败即停，自动递归创建父目录）
(define-library (liii pi code config json-write)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii string)
    (liii path)
    (liii json)
  ) ;import
  (export pi-config-json-pretty pi-write-atomic pi-apply-writes)
  (begin

    ;; ---------- JSON 序列化（多行、2 空格缩进，保序） ----------

    ;; 委托 (liii json) 的 json->string :indent 2（1244 引入的格式化选项）：
    ;; 键序保持、数组逐元素展开、'() 序列化为 {}，与 bin/gf json
    ;; 及上游 JSON.stringify(x, null, 2) 的多行风格一致
    (define (pi-config-json-pretty v)
      (json->string v :indent 2)
    ) ;define

    (define (pi-write-atomic abs-path text)
      (let* ((tmp (string-append abs-path ".tmp")) (bak (string-append abs-path ".bak")))
        (path-write-text (path tmp) text)
        (if (file-exists? abs-path)
          (begin
            (path-unlink (path bak) #t)
            (path-rename (path abs-path) (path bak))
            (path-rename (path tmp) (path abs-path))
            (path-unlink (path bak) #t)
          ) ;begin
          (path-rename (path tmp) (path abs-path))
        ) ;if
      ) ;let*
    ) ;define

    (define (%abs dir-str rel)
      (string-append dir-str "/" rel)
    ) ;define

    (define (pi-apply-writes dir-str writes)
      (let loop
        ((rest writes) (done '()))
        (if (null? rest)
          (list (cons "ok" #t)
            (cons "message" (string-append "saved " (string-join (reverse done) ", ")))
            (cons "failed" #f)
          ) ;list
          (let* ((entry (car rest)) (rel (car entry)) (abs (%abs dir-str rel)))
            (catch #t
              (lambda ()
                (let ((dir (path-parent (path abs))))
                  (unless (path-exists? dir)
                    (path-mkdir dir :parents #t :exist-ok #t)
                  ) ;unless
                ) ;let
                (pi-write-atomic abs (cdr entry))
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
