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
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

;; 语言处理器抽象层：把"按语言收集文件、格式化、逐文件检查"做成通用框架，
;; 各语言（scheme-fmt / cpp-fmt / 未来的 python-fmt ...）只实现一组同名过程并注册进来。
;;
;; handler 是一个关联列表，键为符号。统一协议：
;;   ((name         . 'scheme)
;;    (label        . "Scheme")                 ; 人类可读名，用于日志
;;    (extensions   . (".scm" ".sld"))          ; 该语言处理的后缀列表（带点）
;;    (collect      . (lambda (cfg) ...))       ; 从 cfg 的 path 收集文件列表（仓库批量）
;;    (format-files . (lambda (files cfg) ...)) ; 批量格式化，返回 (total updated cached)
;;    (format-file  . (lambda (path dry-run excludes) ...))        ; 单文件格式化
;;    (format-directory . (lambda (dir exts excludes dry-run) ...)); 目录递归格式化
;;    (check-file   . (lambda (path cfg) ...))) ; 单文件检查 -> #t(已格式化)/#f
;; 仓库批量/check 的方法接 cfg（handler 内部用 goldfmt-config 访问器自取本语言配置）；
;; 单文件/目录的方法接调用方传入的后缀/excludes（带路径参数模式无完整 cfg）。
;; 主入口按文件后缀查注册表派发，不硬编码任何语言。新增语言只需注册 handler。
;; 通用 exclude 匹配（精确 + glob *）与文件收集也在此提供，供各 handler 复用。

(define-library (liii goldfmt lang)
  (import (liii base) (liii path) (liii string) (liii goldfmt path))
  (export register-lang! lang-list lang-ref lang-name lang-label lang-extensions
    extensions-for-lang-name lang-for-extension lang-for-extensions
    lang-for-name path-matches-exclude? file-excluded? collect-files
    collect-files-dfs fmt-extract-error-message print-fmt-failure
    print-fmt-result-line
  ) ;export
  (begin

    ;; ---- 注册表 ---------------------------------------------------------
    ;; 内部可变状态：handler 列表，按注册（模块 import）先后顺序排列。
    ;; 主入口仓库批量 / check 时遍历此列表派发；新增语言只需 import 新模块即自动加入。
    (define %lang-registry '())

    ;; 追加到末尾（保持 import 顺序）；同名 handler 替换旧的。
    (define (register-lang! handler)
      (let ((name (cdr (assq 'name handler))))
        (set! %lang-registry
          (let loop
            ((rs %lang-registry) (acc '()))
            (if (null? rs)
              (reverse (cons handler acc))
              (if (eq? (lang-name (car rs)) name)
                (loop (cdr rs) (cons handler acc))
                (loop (cdr rs) (cons (car rs) acc))
              ) ;if
            ) ;if
          ) ;let
        ) ;set!
        handler
      ) ;let
    ) ;define

    ;; 按注册顺序返回 handler 列表。
    (define (lang-list)
      %lang-registry
    ) ;define

    ;; handler 的 name 字段（符号）。
    (define (lang-name handler)
      (cdr (assq 'name handler))
    ) ;define

    ;; handler 的 label 字段（人类可读名）；未提供时回退为 name。
    (define (lang-label handler)
      (let ((p (assq 'label handler)))
        (if p (cdr p) (lang-name handler))
      ) ;let
    ) ;define

    ;; handler 字段访问器：取某方法。
    (define (lang-ref handler key)
      (cdr (assq key handler))
    ) ;define

    ;; handler 的 extensions 字段（后缀列表，带点）。
    (define (lang-extensions handler)
      (cdr (assq 'extensions handler))
    ) ;define

    ;; token（如 "cpp"/"scheme"）若是已注册语言名，返回该语言后缀列表；否则 #f。
    ;; 供 -e 按语言名展开（-e cpp => cpp 语言全部后缀）。
    (define (extensions-for-lang-name token)
      (let loop
        ((handlers (lang-list)))
        (if (null? handlers)
          #f
          (if (eq? (lang-name (car handlers)) (string->symbol token))
            (lang-extensions (car handlers))
            (loop (cdr handlers))
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    ;; ext（单个带点后缀，如 ".cpp"）是否由 handler 接管。
    (define (handler-matches-ext? handler ext)
      (let loop
        ((exts (lang-extensions handler)))
        (if (null? exts) #f (if (string=? (car exts) ext) #t (loop (cdr exts))))
      ) ;let
    ) ;define

    ;; 按单个后缀查 handler：遍历注册表返回第一个 extensions 含 ext 的 handler；
    ;; 都不匹配则返回 #f（调用方自行决定默认/报错）。
    (define (lang-for-extension ext)
      (let loop
        ((handlers (lang-list)))
        (if (null? handlers)
          #f
          (if (handler-matches-ext? (car handlers) ext)
            (car handlers)
            (loop (cdr handlers))
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    ;; 按后缀列表查 handler：返回 extensions 与 exts 有交集的 handler 列表
    ;; （目录模式可能 -e 指定多语言后缀）。
    (define (lang-for-extensions exts)
      (let loop
        ((handlers (lang-list)) (acc '()))
        (if (null? handlers)
          (reverse acc)
          (let ((h (car handlers)))
            (loop (cdr handlers)
              (if
                (let any
                  ((es exts))
                  (if (null? es) #f (if (handler-matches-ext? h (car es)) #t (any (cdr es))))
                ) ;let
                (cons h acc)
                acc
              ) ;if
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 按语言名（符号）查 handler：返回第一个 name 匹配的 handler，无则 #f。
    (define (lang-for-name name)
      (let loop
        ((handlers (lang-list)))
        (if (null? handlers)
          #f
          (if (eq? (lang-name (car handlers)) name) (car handlers) (loop (cdr handlers)))
        ) ;if
      ) ;let
    ) ;define

    ;; ---- 共享的错误消息与打印辅助 ----------------------------------------
    ;; 供 scheme-fmt / stem-fmt / cpp-fmt 及其 (liii go) worker 会话共用
    ;; （worker 只能调用导出符号）。原先 scheme 与 stem 各持一份同文实现，
    ;; 并发改造时收敛于此，避免双份维护漂移。

    ;; 从 catch 的 (tag . info) 提取简短错误消息。
    (define (fmt-extract-error-message tag info)
      (let ((raw-msg
              (cond
               ((and (pair? info) (string? (car info))) (car info))
               ((string? tag) tag)
               ((symbol? tag) (symbol->string tag))
               (else (object->string tag #f))
              ) ;cond
            ) ;raw-msg
           ) ;
        (cond ((string-starts? raw-msg "unexpected close paren") "unexpected close paren")
              ((string-starts? raw-msg "missing close paren") "missing close paren")
              (else
                (string-trim-right raw-msg
                  (lambda (c) (or (char=? c #\:) (char=? c #\space) (char=? c #\newline)))
                ) ;string-trim-right
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (paren-error? msg)
      (or (string-contains? msg "unexpected close paren")
        (string-contains? msg "missing close paren")
      ) ;or
    ) ;define

    ;; 打印失败行（Failed + 可选 gf fix 提示）并 flush。原各语言 format-file
    ;; 失败路径的输出；统一 flush（不改变输出字节，仅时序）。
    (define (print-fmt-failure file msg)
      (display (string-append "  Failed: " file ": " msg))
      (newline)
      (when (paren-error? msg)
        (display (string-append "Hint: try `gf fix "
                   file
                   "` to repair common parenthesis issues."
                 ) ;string-append
        ) ;display
        (newline)
      ) ;when
      (flush-output-port (current-output-port))
    ) ;define

    ;; 主线程按结果打印一个文件的处理行（任务 1609 无序 Worker Pool 的输出
    ;; 契约：只有 Updated 与 Failed 打印明细，Unchanged 与 Cached 静默不打印，
    ;; 统计见各语言汇总行）。
    (define (print-fmt-result-line file status msg)
      (cond ((eq? status 'updated) (display (string-append "  Updated: " file)) (newline))
            ((eq? status 'failed) (print-fmt-failure file msg))
            (else #f)
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
