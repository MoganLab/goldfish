;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core skills format)
  (export format-skills-for-prompt expand-skill-command parse-skill-block)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii pi code core skills types)
    (liii pi code core skills frontmatter)
  ) ;import
  (begin

    (define (%escape-xml str)
      (let* ((s1 (string-replace str "&" "&amp;"))
             (s2 (string-replace s1 "<" "&lt;"))
             (s3 (string-replace s2 ">" "&gt;"))
             (s4 (string-replace s3 "\"" "&quot;"))
             (s5 (string-replace s4 "'" "&apos;"))
            ) ;
        s5
      ) ;let*
    ) ;define

    ;; 格式化技能列表注入系统提示词
    ;; 排除 disable-model-invocation 技能；若可见技能为空返回空字符串；
    ;; 否则生成包含 read 工具指引与 <available_skills> XML 结构的 <skills> 根区块
    (define (format-skills-for-prompt skills)
      (let ((visible
              (filter (lambda (s) (not (pi-skill-disable-model-invocation? s))) skills)
            ) ;visible
           ) ;
        (if (null? visible)
          ""
          (let* ((header (string-append "<skills>\n"
                           "The following skills provide specialized instructions for specific tasks.\n"
                           "Use the read tool to load a skill's file when the task matches its description.\n"
                           "Resolve relative paths against the skill's directory (parent of SKILL.md). Use absolute paths in tool calls.\n\n"
                           "<available_skills>"
                         ) ;string-append
                 ) ;header
                 (skill-xmls
                   (map
                     (lambda (s)
                       (string-append "  <skill>\n"
                         "    <name>"
                         (%escape-xml (pi-skill-name s))
                         "</name>\n"
                         "    <description>"
                         (%escape-xml (pi-skill-description s))
                         "</description>\n"
                         "    <location>"
                         (%escape-xml (pi-skill-file-path s))
                         "</location>\n"
                         "  </skill>"
                       ) ;string-append
                     ) ;lambda
                     visible
                   ) ;map
                 ) ;skill-xmls
                 (footer "</available_skills>\n</skills>")
                ) ;
            (string-append header "\n" (string-join skill-xmls "\n") "\n" footer)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 展开 /skill:name 命令为完整正文消息
    (define (expand-skill-command text skills)
      (if (not (string-starts? text "/skill:"))
        text
        (let* ((after-colon (substring text 7 (string-length text)))
               (space-idx (string-position " " after-colon))
               (name (if space-idx (substring after-colon 0 space-idx) after-colon))
               (raw-args (if space-idx
                           (substring after-colon (+ space-idx 1) (string-length after-colon))
                           ""
                         ) ;if
               ) ;raw-args
               (args (string-trim-both raw-args))
               (skill
                 (find (lambda (s) (string=? (pi-skill-name s) name)) skills)
               ) ;skill
              ) ;
          (if (not skill)
            text
            (catch #t
              (lambda ()
                (let* ((content (path-read-text (pi-skill-file-path skill)))
                       (body (string-trim-both (strip-frontmatter content)))
                       (skill-block (string-append "<skill name=\""
                                      (pi-skill-name skill)
                                      "\" location=\""
                                      (pi-skill-file-path skill)
                                      "\">\n"
                                      "References are relative to "
                                      (pi-skill-base-dir skill)
                                      ".\n\n"
                                      body
                                      "\n"
                                      "</skill>"
                                    ) ;string-append
                       ) ;skill-block
                      ) ;
                  (if (string-null? args) skill-block (string-append skill-block "\n\n" args))
                ) ;let*
              ) ;lambda
              (lambda (tag . args) text)
            ) ;catch
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    ;; 解析用户消息中的技能展开块，逆向切分出 name、content 与 user-message
    (define (parse-skill-block text)
      (let ((open-prefix "<skill name=\""))
        (if (not (string-starts? text open-prefix))
          #f
          (let ((tag-close-pos (string-position ">\n" text)))
            (if (not tag-close-pos)
              #f
              (let* ((tag (substring text 0 (+ tag-close-pos 1)))
                     (name-pos (string-position "name=\"" tag))
                     (name-start (if name-pos (+ name-pos 6) #f))
                     (name-end (if name-start (string-position "\"" tag name-start) #f))
                     (name (if (and name-start name-end) (substring tag name-start name-end) ""))
                     (close-tag "\n</skill>")
                     (close-pos (string-position close-tag text (+ tag-close-pos 2)))
                    ) ;
                (if (not close-pos)
                  #f
                  (let* ((content (substring text (+ tag-close-pos 2) close-pos))
                         (after-close (+ close-pos (string-length close-tag)))
                         (rest (if (< after-close (string-length text)) (substring text after-close) ""))
                         (trimmed-rest (string-trim-both rest))
                         (user-msg (if (string-null? trimmed-rest) #f trimmed-rest))
                        ) ;
                    (list (cons 'name name) (cons 'content content) (cons 'user-message user-msg))
                  ) ;let*
                ) ;if
              ) ;let*
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
