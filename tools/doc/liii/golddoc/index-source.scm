;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; 扫描加载根目录下的库源码，抽取 (define-library ... (export ...)) 中
;; 导出的函数名，生成 "函数名 → 库名" 索引。

(define-library (liii golddoc index-source)
  (import (scheme base)
    (scheme read)
    (liii golddoc library)
    (liii golddoc render)
    (liii os)
    (liii path)
    (liii sort)
    (liii string)
  ) ;import
  (export build-index-for-load-roots index->json-value)
  (begin

    (define (append-unique-string strings value)
      (if (or (not (string? value)) (member value strings))
        strings
        (append strings (list value))
      ) ;if
    ) ;define

    (define (supported-test-group? group)
      (not (excluded-test-group? group))
    ) ;define

    ;; 测试目录不是库源码目录：库迁移到 library/ 之后，tests/ 会与库源码
    ;; 同处一个加载根目录下，扫描时必须跳过，否则 *-test.scm 会被当成库源码
    ;; 解析（部分测试文件使用 #"" 原始字符串等读取扩展，会让索引构建直接失败）。
    (define (test-directory-name? name)
      (or (string=? name "tests") (string=? name "test"))
    ) ;define

    (define (test-source-file-name? name)
      (string-ends? name "-test.scm")
    ) ;define

    (define (sorted-dir-entries dir)
      (list-sort string<? (vector->list (listdir dir)))
    ) ;define

    (define (index-add! index function-name library-entry)
      (let ((cell (assoc function-name index)))
        (if cell
          (set-cdr! cell (append-unique-string (cdr cell) library-entry))
          (set! index (append index (list (cons function-name (list library-entry)))))
        ) ;if
        index
      ) ;let
    ) ;define

    (define (index-add-exported-functions index library-entry exported-functions)
      (let loop
        ((remaining exported-functions) (result index))
        (if (null? remaining)
          result
          (loop (cdr remaining) (index-add! result (car remaining) library-entry))
        ) ;if
      ) ;let
    ) ;define

    (define (sorted-index index)
      (map
        (lambda (entry) (cons (car entry) (list-sort string<? (cdr entry))))
        (list-sort (lambda (left right) (string<? (car left) (car right))) index)
      ) ;map
    ) ;define

    (define (library-form? form)
      (and (pair? form) (eq? (car form) 'define-library) (pair? (cdr form)))
    ) ;define

    (define (export-form? form)
      (and (pair? form) (eq? (car form) 'export))
    ) ;define

    (define (library-name-part->string value)
      (cond ((symbol? value) (symbol->string value))
            ((number? value) (number->string value))
            (else #f)
      ) ;cond
    ) ;define

    (define (library-name->entry library-name)
      (if
        (not (and (list? library-name) (>= (length library-name) 2)))
        #f
        (let ((group (library-name-part->string (car library-name))))
          (if (or (not group) (not (supported-test-group? group)))
            #f
            (let loop
              ((rem (cdr library-name)) (acc (list group)))
              (if (null? rem)
                (string-append "(" (string-join acc " ") ")")
                (let ((part (library-name-part->string (car rem))))
                  (if (not part) #f (loop (cdr rem) (append acc (list part))))
                ) ;let
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (rename-export-spec? value)
      (and (pair? value) (eq? (car value) 'rename) (= (length value) 3))
    ) ;define

    (define (export-spec->name spec)
      (cond ((symbol? spec) (symbol->string spec))
            ((rename-export-spec? spec) (library-name-part->string (caddr spec)))
            (else #f)
      ) ;cond
    ) ;define

    (define (export-form-names form)
      (let loop
        ((remaining (cdr form)) (names '()))
        (if (null? remaining)
          names
          (let ((name (export-spec->name (car remaining))))
            (loop (cdr remaining) (if name (append-unique-string names name) names))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (define-library-exported-names form)
      (let loop
        ((remaining (cddr form)) (names '()))
        (if (null? remaining)
          names
          (let ((declaration (car remaining)))
            (loop (cdr remaining)
              (if (export-form? declaration)
                (let export-loop
                  ((exports (export-form-names declaration)) (result names))
                  (if (null? exports)
                    result
                    (export-loop (cdr exports) (append-unique-string result (car exports)))
                  ) ;if
                ) ;let
                names
              ) ;if
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (library-name-group library-name)
      (if (and (pair? library-name) (pair? (cdr library-name)))
        (library-name-part->string (car library-name))
        #f
      ) ;if
    ) ;define

    ;; 只索引与所在分组目录同名的库：加载根目录下可能存在非库目录
    ;; （例如安装后的 share/goldfish 同时含有 liii/ 与 library/），
    ;; 按目录名过滤可避免把它们误记成 (library ...) / (tools ...) 之类的库。
    (define (index-add-library-form index group-name form)
      (if (not (library-form? form))
        index
        (let* ((library-name (cadr form))
               (library-entry (library-name->entry library-name))
               (group (library-name-group library-name))
              ) ;
          (if (and library-entry group (string=? group group-name))
            (index-add-exported-functions index
              library-entry
              (define-library-exported-names form)
            ) ;index-add-exported-functions
            index
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (warn-unreadable-source library-file)
      (stderr-line (string-append "Warning: skipped unreadable library source: " library-file)
      ) ;stderr-line
    ) ;define

    ;; 预筛选：文件里不出现 "(<分组名>" 时不可能定义该分组的库，
    ;; 直接跳过而不必解析，避免加载根目录下的无关目录（tools/、demo/ 等）
    ;; 拖慢索引构建，也避免无关文件因读取扩展而报错。
    (define (source-file-may-define-library? library-file group-name)
      (guard (ex (#t #t))
        (string-contains? (path-read-text library-file) (string-append "(" group-name))
      ) ;guard
    ) ;define

    (define (index-add-source-file index group-name library-file)
      (if (not (source-file-may-define-library? library-file group-name))
        index
        (guard (ex (#t (warn-unreadable-source library-file) index))
          (call-with-input-file library-file
            (lambda (port)
              (let loop
                ((result index))
                (let ((form (read port)))
                  (if (eof-object? form)
                    result
                    (loop (index-add-library-form result group-name form))
                  ) ;if
                ) ;let
              ) ;let
            ) ;lambda
          ) ;call-with-input-file
        ) ;guard
      ) ;if
    ) ;define

    (define (find-scheme-source-files dir)
      (let collect
        ((current-dir dir) (result '()))
        (let loop
          ((entries (sorted-dir-entries current-dir)) (acc result))
          (if (null? entries)
            acc
            (let* ((name (car entries)) (full-path (path->string (path-join current-dir name))))
              (cond ((and (path-dir? full-path) (test-directory-name? name))
                     (loop (cdr entries) acc)
                    ) ;
                    ((path-dir? full-path) (loop (cdr entries) (collect full-path acc)))
                    ((and (path-file? full-path)
                       (string-ends? name ".scm")
                       (not (test-source-file-name? name))
                     ) ;and
                     (loop (cdr entries) (append acc (list full-path)))
                    ) ;
                    (else (loop (cdr entries) acc))
              ) ;cond
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (build-index-for-load-root load-root)
      (let ((index '()))
        (for-each
          (lambda (group-name)
            (if (and (not (test-directory-name? group-name)) (supported-test-group? group-name))
              (let ((group-dir (path->string (path-join load-root group-name))))
                (if (path-dir? group-dir)
                  (for-each (lambda (source-file)
                              (set! index (index-add-source-file index group-name source-file))
                            ) ;lambda
                    (find-scheme-source-files group-dir)
                  ) ;for-each
                  #f
                ) ;if
              ) ;let
              #f
            ) ;if
          ) ;lambda
          (sorted-dir-entries load-root)
        ) ;for-each
        (sorted-index index)
      ) ;let
    ) ;define

    (define (merge-index-entries index entries)
      (let ((merged index))
        (for-each
          (lambda (entry)
            (let* ((function-name (car entry))
                   (libraries (cdr entry))
                   (cell (assoc function-name merged))
                  ) ;
              (if cell
                (for-each
                  (lambda (library-entry)
                    (set-cdr! cell (append-unique-string (cdr cell) library-entry))
                  ) ;lambda
                  libraries
                ) ;for-each
                (set! merged (append merged (list (cons function-name libraries))))
              ) ;if
            ) ;let*
          ) ;lambda
          entries
        ) ;for-each
        merged
      ) ;let
    ) ;define

    (define (build-index-for-load-roots load-roots)
      (let loop
        ((remaining load-roots) (index '()))
        (if (null? remaining)
          (sorted-index index)
          (loop (cdr remaining)
            (merge-index-entries index (build-index-for-load-root (car remaining)))
          ) ;loop
        ) ;if
      ) ;let
    ) ;define

    (define (index->json-value index)
      (map
        (lambda (entry) (cons (car entry) (list->vector (cdr entry))))
        index
      ) ;map
    ) ;define

  ) ;begin
) ;define-library
