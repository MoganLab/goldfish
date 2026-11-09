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
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; (hint liii json) - (liii json) 的类型接口
;;
;; 本库只提供类型签名数据，不导出 (liii json) 的任何过程，
;; 也不导入其他 hint 库，因此可以被独立加载。
;;
;; (liii json) 自身实现了全部导出过程，签名均在本库声明。
;;
;; 类型约定：
;;   - JSON 值没有名义类型（库未提供 json? 谓词），统一用 any? 表示；
;;   - 键统一用 any? 表示：可以是 symbol? / string? / integer? / boolean?，
;;     json-set / json-push / json-reduce 还接受 #t / #f / 过程作为匹配键；
;;   - 安全获取器（json-ref-string 等）的默认值常为 #f，
;;     参数与返回值都用 (or #f T) 表示；
;;   - 可变参数过程按常见调用形态登记若干条签名，
;;     关键字参数以 any? 占位（hint 格式暂不支持关键字类型）。

(define-library (hint liii json)
  (import (scheme base))
  (export hint-liii/json)
  (begin

    ;; ; ---------------------------------------------------------
    ;; ; 0. 序列化与解析
    ;; ; ---------------------------------------------------------

    ;; json-string-escape : (string? -> string?)
    (define hint-json-string-escape '((string? -> string?)))

    ;; string->json : (string? -> any?)
    (define hint-string->json '((string? -> any?)))

    ;; json->string : (any? -> string?)
    ;;               (any? (or #f integer? string?) -> string?)
    ;;               (any? (or #f integer? string?) boolean? -> string?)
    ;;               (any? (or #f integer? string?) boolean? boolean? -> string?)
    ;;               (any? (or #f integer? string?) boolean? boolean? boolean? -> string?)
    ;; define* 的可选参数：indent 为非负整数 / 缩进字符串 / #f，
    ;; sort-keys / ensure-ascii / compact 均为布尔值。
    ;; 可选参数既可按位置传入，也可按关键字传入（:indent 等），
    ;; 关键字形态下实参与位置的对应关系不确定，检查器对含关键字的调用
    ;; 保守跳过实参类型检查，故此处按位置形态登记。
    (define hint-json->string
      '((any? -> string?)
        (any? (or #f integer? string?) -> string?)
        (any? (or #f integer? string?) boolean? -> string?)
        (any? (or #f integer? string?) boolean? boolean? -> string?)
        (any? (or #f integer? string?) boolean? boolean? boolean? -> string?))
    ) ;define

    ;; read-json : (-> any?)        ; 读当前输入端口
    ;;             (input-port? -> any?)
    (define hint-read-json '((-> any?) (input-port? -> any?)))

    ;; write-json : (any? -> any?)                      ; 无附加参数
    ;;               (any? output-port? -> any?)        ; 指定输出端口
    ;;               (any? any? any? -> any?)           ; 一个关键字参数
    ;;               (any? output-port? any? any? -> any?) ; 端口 + 关键字参数
    (define hint-write-json
      '((any? -> any?)
        (any? output-port? -> any?)
        (any? any? any? -> any?)
        (any? output-port? any? any? -> any?))
    ) ;define

    ;; ; ---------------------------------------------------------
    ;; ; 1. 路径读写
    ;; ; ---------------------------------------------------------

    ;; json-ref : (any? any? -> any?)             ; 单键
    ;;            (any? any? any? -> any?)        ; 两级键路径
    (define hint-json-ref '((any? any? -> any?) (any? any? any? -> any?)))

    ;; json-set : (any? any? any? -> any?)        ; 单键 + 值
    ;;            (any? any? any? any? -> any?)   ; 两级键路径 + 值
    (define hint-json-set '((any? any? any? -> any?)
                            (any? any? any? any? -> any?)))

    ;; json-push : (any? any? any? -> any?)       ; 单键 + 值
    ;;             (any? any? any? any? -> any?)  ; 两级键路径 + 值
    (define hint-json-push
      '((any? any? any? -> any?) (any? any? any? any? -> any?))
    ) ;define

    ;; json-drop : (any? any? -> any?)            ; 单键
    ;;             (any? any? any? -> any?)       ; 两级键路径
    (define hint-json-drop '((any? any? -> any?) (any? any? any? -> any?)))

    ;; json-reduce : (any? any? procedure? -> any?)          ; 单键 + 变换过程
    ;;               (any? any? procedure? any? -> any?)     ; 两级键路径 + 变换过程
    (define hint-json-reduce
      '((any? any? procedure? -> any?) (any? any? procedure? any? -> any?))
    ) ;define

    ;; ; ---------------------------------------------------------
    ;; ; 2. 类型谓词
    ;; ; ---------------------------------------------------------

    ;; json-null? : (any? -> boolean?)
    (define hint-json-null? '((any? -> boolean?)))

    ;; json-object? : (any? -> boolean?)
    (define hint-json-object? '((any? -> boolean?)))

    ;; json-array? : (any? -> boolean?)
    (define hint-json-array? '((any? -> boolean?)))

    ;; json-string? : (any? -> boolean?)
    (define hint-json-string? '((any? -> boolean?)))

    ;; json-float? : (any? -> boolean?)
    (define hint-json-float? '((any? -> boolean?)))

    ;; json-number? : (any? -> boolean?)
    (define hint-json-number? '((any? -> boolean?)))

    ;; json-integer? : (any? -> boolean?)
    (define hint-json-integer? '((any? -> boolean?)))

    ;; json-boolean? : (any? -> boolean?)
    (define hint-json-boolean? '((any? -> boolean?)))

    ;; ; ---------------------------------------------------------
    ;; ; 3. 状态检查与安全获取器
    ;; ; ---------------------------------------------------------

    ;; json-contains-key? : (any? any? -> boolean?)
    (define hint-json-contains-key? '((any? any? -> boolean?)))

    ;; json-get-or-else : (any? any? -> any?)
    (define hint-json-get-or-else '((any? any? -> any?)))

    ;; json-ref-string : (any? any? (or #f string?) -> (or #f string?))
    (define hint-json-ref-string '((any? any?
                                     (or #f string?)
                                     ->
                                     (or #f string?))))

    ;; json-ref-number : (any? any? (or #f number?) -> (or #f number?))
    (define hint-json-ref-number '((any? any?
                                     (or #f number?)
                                     ->
                                     (or #f number?))))

    ;; json-ref-integer : (any? any? (or #f integer?) -> (or #f integer?))
    (define hint-json-ref-integer
      '((any? any? (or #f integer?) -> (or #f integer?)))
    ) ;define

    ;; json-ref-boolean : (any? any? (or #f boolean?) -> (or #f boolean?))
    (define hint-json-ref-boolean
      '((any? any? (or #f boolean?) -> (or #f boolean?)))
    ) ;define

    ;; ; ---------------------------------------------------------
    ;; ; 4. 辅助工具
    ;; ; ---------------------------------------------------------

    ;; json-keys : (any? -> list?)
    (define hint-json-keys '((any? -> list?)))

    (define (hint-liii/json name)
      (case name
       ((json-string-escape) hint-json-string-escape)
       ((string->json) hint-string->json)
       ((json->string) hint-json->string)
       ((read-json) hint-read-json)
       ((write-json) hint-write-json)
       ((json-ref) hint-json-ref)
       ((json-set) hint-json-set)
       ((json-push) hint-json-push)
       ((json-drop) hint-json-drop)
       ((json-reduce) hint-json-reduce)
       ((json-null?) hint-json-null?)
       ((json-object?) hint-json-object?)
       ((json-array?) hint-json-array?)
       ((json-string?) hint-json-string?)
       ((json-float?) hint-json-float?)
       ((json-number?) hint-json-number?)
       ((json-integer?) hint-json-integer?)
       ((json-boolean?) hint-json-boolean?)
       ((json-contains-key?) hint-json-contains-key?)
       ((json-get-or-else) hint-json-get-or-else)
       ((json-ref-string) hint-json-ref-string)
       ((json-ref-number) hint-json-ref-number)
       ((json-ref-integer) hint-json-ref-integer)
       ((json-ref-boolean) hint-json-ref-boolean)
       ((json-keys) hint-json-keys)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
