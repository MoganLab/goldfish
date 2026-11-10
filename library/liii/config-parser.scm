;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;  http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; Inspired by Python's configparser module

(define-library (liii config-parser)
  (import (scheme base)
    (scheme char)
    (scheme file)
    (liii base)
    (liii hash-table)
    (liii error)
    (srfi srfi-128)
  ) ;import
  (export make-config-parser config-parser? config-read-string config-read-file
    config-sections config-has-section? config-has-option? config-get
    config-get-int config-get-boolean config-options config-items config-set!
    config-add-section! config-remove-section! config-remove-option!
    config-write
  ) ;export
  (begin

    (define-record-type <config-parser>
      (%make-config-parser sections)
      config-parser?
      (sections config-parser-sections)
    ) ;define-record-type
    (define string-comparator
      (make-comparator string? string=? string<? string-hash)
    ) ;define
    (define *boolean-true* '("yes" "true" "on" "1"))
    (define *boolean-false* '("no" "false" "off" "0"))
    (define (string-in-list? str lst)
      (let ((lower (string-downcase str)))
        (let loop
          ((rest lst))
          (cond ((null? rest) #f)
                ((string=? lower (car rest)) #t)
                (else (loop (cdr rest)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (config-error msg)
      (error 'config-error msg)
    ) ;define

    ;; DEFAULT 节名：特殊节，所有节共享其选项
    (define *default-section-name* "DEFAULT")

    ;; 取节哈希表；不存在时抛错
    (define (require-section config section)
      (let ((sec (hash-table-ref/default (config-parser-sections config) section #f)))
        (if sec sec (config-error (string-append "No section: " section)))
      ) ;let
    ) ;define
    (define (find-first-of str chars)
      (let ((len (string-length str)))
        (let loop
          ((i 0))
          (cond ((= i len) #f)
                ((member (string-ref str i) chars) i)
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    ;; 跳过 str 开头的连续空白，返回首个非空白下标
    (define (skip-leading-spaces str start)
      (let ((len (string-length str)))
        (let loop
          ((i start))
          (if (and (< i len) (char-whitespace? (string-ref str i))) (loop (+ i 1)) i)
        ) ;let
      ) ;let
    ) ;define

    ;; 从 from 往前回退连续空白，返回最后一个非空白的下标
    (define (skip-trailing-spaces str from)
      (if (and (>= from 0) (char-whitespace? (string-ref str from)))
        (skip-trailing-spaces str (- from 1))
        from
      ) ;if
    ) ;define
    (define (ini-string-trim str)
      (let* ((len (string-length str))
             (start (skip-leading-spaces str 0))
             (end (skip-trailing-spaces str (- len 1)))
            ) ;
        (if (>= end start) (substring str start (+ end 1)) "")
      ) ;let*
    ) ;define
    (define (remove-comment line)
      (let ((idx (find-first-of line '(#\; #\#))))
        (if idx (substring line 0 idx) line)
      ) ;let
    ) ;define
    (define (split-at-first-sep str sep-chars)
      (let ((idx (find-first-of str sep-chars)))
        (if idx
          (cons (substring str 0 idx) (substring str (+ idx 1) (string-length str)))
          #f
        ) ;if
      ) ;let
    ) ;define
    (define (parse-section-header line)
      (let* ((trimmed (ini-string-trim line)) (len (string-length trimmed)))
        (if
          (and (> len 1)
            (char=? (string-ref trimmed 0) #\[)
            (char=? (string-ref trimmed (- len 1)) #\])
          ) ;and
          (ini-string-trim (substring trimmed 1 (- len 1)))
          #f
        ) ;if
      ) ;let*
    ) ;define
    (define (make-config-parser)
      (%make-config-parser (make-hash-table string-comparator))
    ) ;define
    (define (ensure-section config section-name)
      (let ((existing (hash-table-ref/default (config-parser-sections config) section-name #f)
            ) ;existing
           ) ;
        (or existing
          (let ((new-sec (make-hash-table string-comparator)))
            (hash-table-set! (config-parser-sections config) section-name new-sec)
            new-sec
          ) ;let
        ) ;or
      ) ;let
    ) ;define
    (define (config-parse-port config inport)
      (let loop
        ((current-section #f))
        (let ((line (read-line inport)))
          (if (eof-object? line)
            config
            (parse-one-line! config line current-section loop)
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 解析单行后以新的 current-section 继续循环；分派顺序：
    ;; 空行跳过 -> 节头切换 -> key=value 落盘 -> 其余忽略
    (define (parse-one-line! config line current-section continue)
      (let* ((no-comment (remove-comment line)) (trimmed (ini-string-trim no-comment)))
        (if (string=? trimmed "")
          (continue current-section)
          (let ((section-name (parse-section-header trimmed)))
            (if section-name
              (begin
                (ensure-section config section-name)
                (continue section-name)
              ) ;begin
              (begin
                (parse-key-value! config trimmed current-section)
                (continue current-section)
              ) ;begin
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    ;; 行为 key=value / key:value 时写入当前节（无当前节则忽略）
    (define (parse-key-value! config trimmed current-section)
      (let ((kv (split-at-first-sep trimmed '(#\= #\:))))
        (when (and kv current-section)
          (let ((key (ini-string-trim (car kv))) (val (ini-string-trim (cdr kv))))
            (hash-table-set! (ensure-section config current-section)
              (string-downcase key)
              val
            ) ;hash-table-set!
          ) ;let
        ) ;when
      ) ;let
    ) ;define
    (define (config-read-string config str)
      (config-parse-port config (open-input-string str))
    ) ;define
    (define (config-read-file config path)
      (let ((port (open-input-file path)))
        (config-parse-port config port)
        (close-input-port port)
      ) ;let
    ) ;define
    (define (get-default-section config)
      (hash-table-ref/default (config-parser-sections config)
        *default-section-name*
        #f
      ) ;hash-table-ref/default
    ) ;define

    ;; 在节与 DEFAULT 中查选项值；均无则抛错
    (define (lookup-option config sec section opt option)
      (cond ((hash-table-contains? sec opt) (hash-table-ref/default sec opt #f))
            (else
              (let ((defaults (get-default-section config)))
                (if (and defaults (hash-table-contains? defaults opt))
                  (hash-table-ref/default defaults opt #f)
                  (config-error (string-append "No option '" option "' in section '" section "'"))
                ) ;if
              ) ;let
            ) ;else
      ) ;cond
    ) ;define
    (define (config-sections config)
      (let ((all-keys (hash-table-keys (config-parser-sections config))))
        (let loop
          ((rest all-keys) (result '()))
          (cond ((null? rest) (reverse result))
                ((string=? (car rest) *default-section-name*) (loop (cdr rest) result))
                (else (loop (cdr rest) (cons (car rest) result)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (config-has-section? config section)
      (hash-table-contains? (config-parser-sections config) section)
    ) ;define
    (define (config-has-option? config section option)
      (let ((opt (string-downcase option)))
        (let ((sec (hash-table-ref/default (config-parser-sections config) section #f)))
          (if sec
            (or (hash-table-contains? sec opt)
              (let ((defaults (get-default-section config)))
                (and defaults (hash-table-contains? defaults opt))
              ) ;let
            ) ;or
            #f
          ) ;if
        ) ;let
      ) ;let
    ) ;define
    (define (config-get config section option)
      (let ((opt (string-downcase option)))
        (let ((sec (hash-table-ref/default (config-parser-sections config) section #f)))
          (if sec
            (lookup-option config sec section opt option)
            (config-error (string-append "No section: " section))
          ) ;if
        ) ;let
      ) ;let
    ) ;define
    (define (config-get-int config section option)
      (string->number (config-get config section option))
    ) ;define
    (define (config-get-boolean config section option)
      (let ((val (config-get config section option)))
        (cond ((string-in-list? val *boolean-true*) #t)
              ((string-in-list? val *boolean-false*) #f)
              (else (config-error (string-append "Not a boolean: " val)))
        ) ;cond
      ) ;let
    ) ;define
    (define (config-options config section)
      (let ((sec (require-section config section)))
        (let ((defaults (get-default-section config)))
          (if defaults
            (let ((all-keys (hash-table-keys defaults)))
              (hash-table-for-each
                (lambda (k v)
                  (when (not (member k all-keys))
                    (set! all-keys (cons k all-keys))
                  ) ;when
                ) ;lambda
                sec
              ) ;hash-table-for-each
              all-keys
            ) ;let
            (hash-table-keys sec)
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 把节内选项并入 result 关联表：新键追加，已有键覆盖
    (define (merge-section-into-alist! sec result)
      (hash-table-for-each
        (lambda (k v)
          (let ((existing (assoc k result)))
            (if existing (set-cdr! existing v) (set! result (cons (cons k v) result)))
          ) ;let
        ) ;lambda
        sec
      ) ;hash-table-for-each
      result
    ) ;define

    ;; DEFAULT 与节合并后的选项关联表：DEFAULT 在前，节内覆盖
    (define (merged-items config sec)
      (let ((defaults (get-default-section config)) (result '()))
        (when defaults
          (hash-table-for-each
            (lambda (k v) (set! result (cons (cons k v) result)))
            defaults
          ) ;hash-table-for-each
        ) ;when
        (merge-section-into-alist! sec result)
      ) ;let
    ) ;define
    (define (config-items config section)
      (let ((sec (require-section config section)))
        (merged-items config sec)
      ) ;let
    ) ;define
    (define (config-set! config section option value)
      (let ((sec (require-section config section)))
        (hash-table-set! sec (string-downcase option) value)
      ) ;let
    ) ;define
    (define (config-add-section! config section)
      (if (hash-table-contains? (config-parser-sections config) section)
        (config-error (string-append "Section already exists: " section))
        (hash-table-set! (config-parser-sections config)
          section
          (make-hash-table string-comparator)
        ) ;hash-table-set!
      ) ;if
    ) ;define
    (define (config-remove-section! config section)
      (if (string=? section *default-section-name*)
        (config-error "Cannot remove DEFAULT section")
        (if (hash-table-contains? (config-parser-sections config) section)
          (hash-table-delete! (config-parser-sections config) section)
          (config-error (string-append "No section: " section))
        ) ;if
      ) ;if
    ) ;define
    (define (config-remove-option! config section option)
      (let ((sec (require-section config section)))
        (let ((opt (string-downcase option)))
          (if (hash-table-contains? sec opt)
            (hash-table-delete! sec opt)
            (remove-default-or-error config section option opt)
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 选项不在节内：存在于 DEFAULT 则抛错，否则报无此选项
    (define (remove-default-or-error config section option opt)
      (let ((defaults (get-default-section config)))
        (if (and defaults (hash-table-contains? defaults opt))
          (config-error "Cannot remove DEFAULT option from section")
          (config-error (string-append "No option '" option "' in section '" section "'"))
        ) ;if
      ) ;let
    ) ;define
    ;; 写单个节：头 + 每行 k = v + 尾空行
    (define (write-section section-name options port)
      (display (string-append "[" section-name "]") port)
      (newline port)
      (hash-table-for-each (lambda (k v) (display (string-append k " = " v) port) (newline port))
        options
      ) ;hash-table-for-each
      (newline port)
    ) ;define
    (define (config-write config port)
      (let ((defaults (get-default-section config)))
        (when defaults
          (write-section *default-section-name* defaults port)
        ) ;when
      ) ;let
      (hash-table-for-each (lambda (section-name options)
                             (unless (string=? section-name *default-section-name*)
                               (write-section section-name options port)
                             ) ;unless
                           ) ;lambda
        (config-parser-sections config)
      ) ;hash-table-for-each
    ) ;define

  ) ;begin
) ;define-library
