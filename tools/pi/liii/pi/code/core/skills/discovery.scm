;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core skills discovery)
  (export pi-load-skills)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii pi code core skills types)
    (liii pi code core skills frontmatter)
    (liii pi code core tools read)
    (liii pi code config)
  ) ;import
  (begin

    (define (%display-warning msg)
      (let ((p (if (defined? '*stderr*) *stderr* (current-error-port))))
        (display msg p)
      ) ;let
    ) ;define

    (define (%emit-warnings norm-file errs)
      (for-each (lambda (msg)
                  (%display-warning (string-append "warning: " norm-file ": " msg "\n"))
                ) ;lambda
        errs
      ) ;for-each
    ) ;define

    ;; 从解析后的 frontmatter 构建 <pi-skill> 记录
    (define (%build-skill-from-fm fm norm-file norm-base raw-desc)
      (let* ((name-entry (assoc "name" fm))
             (raw-name (if name-entry (cdr name-entry) #f))
             ;; 未指定 name 且文件为 SKILL.md 时回退到父目录名
             (name
               (if
                 (and (string? raw-name) (not (string-null? (string-trim-both raw-name))))
                 raw-name
                 (path-name (path norm-base))
               ) ;if
             ) ;name
             (name-errs (pi-validate-skill-name name))
             (dis-entry (assoc "disable-model-invocation" fm))
             (disable? (and dis-entry (eq? (cdr dis-entry) #t)))
            ) ;
        (%emit-warnings norm-file name-errs)
        (%make-pi-skill name raw-desc norm-file norm-base disable?)
      ) ;let*
    ) ;define

    ;; 单个文件加载为 <pi-skill>
    (define (load-skill-from-file file-path base-dir)
      (catch #t
        (lambda ()
          (let* ((raw (path-read-text file-path))
                 (norm-file (path->string (pi-abs-path (path->string file-path))))
                 (norm-base (path->string (pi-abs-path (path->string base-dir))))
                ) ;
            (let-values (((fm body) (parse-frontmatter raw)))
              (let* ((desc-entry (assoc "description" fm))
                     (raw-desc (if desc-entry (cdr desc-entry) #f))
                     (has-desc?
                       (and (string? raw-desc) (not (string-null? (string-trim-both raw-desc))))
                     ) ;has-desc?
                    ) ;
                (%emit-warnings norm-file (pi-validate-skill-description raw-desc))
                (if (not has-desc?) #f (%build-skill-from-fm fm norm-file norm-base raw-desc))
              ) ;let*
            ) ;let-values
          ) ;let*
        ) ;lambda
        (lambda (tag . args)
          (%display-warning (string-append "warning: failed to read skill file "
                              (object->string file-path)
                              "\n"
                            ) ;string-append
          ) ;%display-warning
          #f
        ) ;lambda
      ) ;catch
    ) ;define

    (define (%scan-one-entry p entry)
      (cond ((string-starts? entry ".") '())
            ((string=? entry "node_modules") '())
            (else
              (let ((sub-path (path-join p entry)))
                (cond ((path-dir? sub-path) (load-skills-from-dir-internal sub-path))
                      ((and (path-file? sub-path) (string-ends? entry ".md"))
                       (let ((sk (load-skill-from-file sub-path p)))
                         (if sk (list sk) '())
                       ) ;let
                      ) ;
                      (else '())
                ) ;cond
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; 递归扫描目录发现技能
    (define (load-skills-from-dir-internal dir)
      (let ((p (if (path? dir) dir (path dir))))
        (cond
         ((not (and (path-exists? p) (path-dir? p))) '())
         ((path-exists? (path-join p "SKILL.md"))
          ;; 边界保护：包含 SKILL.md 即为技能根目录，停止向下递归
          (let ((sk (load-skill-from-file (path-join p "SKILL.md") p)))
            (if sk (list sk) '())
          ) ;let
         ) ;
         (else (append-map (lambda (entry) (%scan-one-entry p entry))
                 (vector->list (path-list p))
               ) ;append-map
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; 默认技能目录：gf 配置目录（GF_CONFIG_DIR → ~/.gf/agent）优先，
    ;; pi agent 目录（PI_CODING_AGENT_DIR → ~/.pi/agent）兼容，
    ;; 项目级仅查当前目录 .pi/skills（对齐上游）
    (define (%collect-default-dirs cwd)
      (filter string?
        (map (lambda (p) (and (path-exists? p) (path-dir? p) (path->string p)))
          (list (path-join (gf-get-config-dir) "skills")
            (path-join (pi-get-agent-dir) "skills")
            (path-join (path cwd) ".pi" "skills")
          ) ;list
        ) ;map
      ) ;filter
    ) ;define

    (define (%dedup-skills skills)
      (let loop
        ((rem skills) (loaded '()))
        (if (null? rem)
          (reverse loaded)
          (let ((sk (car rem)))
            (cond
             ((find (lambda (s) (equal? (pi-skill-file-path s) (pi-skill-file-path sk)))
                loaded
              ) ;find
              ;; 符号链接指向同一文件：静默跳过
              (loop (cdr rem) loaded)
             ) ;
             ((find (lambda (s) (string=? (pi-skill-name s) (pi-skill-name sk))) loaded)
              ;; 同名冲突：首见胜出（Winner），次者记录 collision 警告
              (%display-warning (string-append "warning: name \"" (pi-skill-name sk) "\" collision\n")
              ) ;%display-warning
              (loop (cdr rem) loaded)
             ) ;
             (else (loop (cdr rem) (cons sk loaded)))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 高层技能加载器：显式路径与默认目录多源合并，
    ;; 冲突去重策略：Winner / Loser 首见胜出，次者输出 collision 警告
    (define* (pi-load-skills (skill-paths '()) (cwd (path-cwd)) (include-defaults #t))
      (let* ((defaults (if include-defaults (%collect-default-dirs cwd) '()))
             (paths (append skill-paths defaults))
             (all-skills (append-map load-skills-from-dir-internal paths))
            ) ;
        (%dedup-skills all-skills)
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
