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

;; 评估 define-library / import 路径上"库名 -> 符号"转换的性能
;; 现状实现: (symbol (object->string libname))
;;   - boot.scm define-library 宏: 库定义时一次
;;   - boot.scm r7rs-import-library-filename: defined? 缓存检查每次 import 一次
;;   - boot.scm import 宏各分支: symbol->value 取库环境每次 import 一次
;; 候选实现: string-append + string->symbol 直接拼接，不走 write 到字符串端口

(import (liii check)
        (liii timeit))

(check-set-mode! 'report-failed)

(define (report title iterations time-val)
  (display "[")
  (display title)
  (display "] iterations=")
  (display iterations)
  (display " total=")
  (display time-val)
  (display "s avg=")
  (display (/ time-val iterations))
  (display "s")
  (newline)
) ;define

;; 候选实现：与 object->string 对库名列表的输出保持一致
;; 库名元素只有符号（liii list）和数字（srfi 1）两种
(define (library-name->symbol libname)
  (string->symbol
    (string-append
      "("
      (let loop ((parts libname) (first #t))
        (if (null? parts)
          ")"
          (string-append
            (if first "" " ")
            (if (symbol? (car parts))
              (symbol->string (car parts))
              (number->string (car parts)))
            (loop (cdr parts) #f))))))
) ;define

;; 正确性：两种实现产出的符号必须一致
(check (library-name->symbol '(liii list)) => (symbol (object->string '(liii list))))
(check (library-name->symbol '(srfi 1)) => (symbol (object->string '(srfi 1))))
(check (library-name->symbol '(scheme base)) => (symbol (object->string '(scheme base))))
;; 产出的符号必须能取到 define-library 定义的全局库环境
(check (symbol->value (library-name->symbol '(liii check))) => (symbol->value (symbol (object->string '(liii check)))))

(display "=== 库名 -> 符号转换性能基准 ===")
(newline)

(define iterations 100000)

(define t1
  (timeit (lambda () (symbol (object->string '(liii list)))) '() iterations))
(report "现状 (symbol (object->string '(liii list)))" iterations t1)

(define t2
  (timeit (lambda () (library-name->symbol '(liii list))) '() iterations))
(report "候选 (library-name->symbol '(liii list))" iterations t2)

(define t3
  (timeit (lambda () (symbol (object->string '(srfi 1)))) '() iterations))
(report "现状 (symbol (object->string '(srfi 1)))" iterations t3)

(define t4
  (timeit (lambda () (library-name->symbol '(srfi 1))) '() iterations))
(report "候选 (library-name->symbol '(srfi 1))" iterations t4)

(newline)
(display "加速比 ((liii list)): ")
(display (/ t1 t2))
(newline)
(display "加速比 ((srfi 1)): ")
(display (/ t3 t4))
(newline)

(newline)
(display "=== 测试完成 ===")
(newline)

;; 第二部分：归因热导入 (import (liii json)) 的各环节开销
;; 对比"注册表直查"方案：define-library 时把库环境按库名列表注册进
;; hash-table,import 时按列表直接查表，完全跳过 字符串转换 + 符号查找
(import (liii json) (liii hash-table))

(newline)
(display "=== 热导入各环节归因（库已加载） ===")
(newline)

(define *library-registry* (make-hash-table))
(hash-table-set! *library-registry* '(liii json)
  (symbol->value (symbol (object->string '(liii json)))))

(define t5
  (timeit (lambda () (defined? (symbol (object->string '(liii json))))) '() iterations))
(report "现状 defined? 缓存检查" iterations t5)

(define t6
  (timeit (lambda () (hash-table-ref *library-registry* '(liii json))) '() iterations))
(report "候选 注册表查库环境" iterations t6)

(define t7
  (timeit (lambda () (symbol->value (symbol (object->string '(liii json))))) '() iterations))
(report "现状 symbol->value 取库环境" iterations t7)

(define t8
  (timeit (lambda () (import (liii json))) '() iterations))
(report "现状 完整热导入 (import (liii json))" iterations t8)

;; 第三部分：归因 r7rs-import-library-filename 与 varlet 的开销
(newline)
(display "=== r7rs-import-library-filename / varlet 归因 ===")
(newline)

(define json-env
  (symbol->value (symbol (object->string '(liii json)))))

(define t9
  (timeit (lambda () (r7rs-import-library-filename '((liii json)))) '() iterations))
(report "现状 r7rs-import-library-filename(已加载)" iterations t9)

(define t10
  (timeit (lambda () (varlet (inlet) json-env)) '() iterations))
(report "现状 varlet 复制库环境" iterations t10)

;; 候选：defined? 命中时跳过文件名拼接（先查缓存再构造文件名）
(define (r7rs-import-library-filename-opt libs)
  (when (pair? libs)
    (let ((lib (if (memq (caar libs) '(only except prefix rename))
                 (cadar libs)
                 (car libs))))
      (when (not (defined? (symbol (object->string lib))))
        (load (let loop ((lib lib) (name ""))
                (set! name (string-append name (symbol->string (car lib))))
                (if (null? (cdr lib))
                  (string-append name ".scm")
                  (begin
                    (set! name (string-append name "/"))
                    (loop (cdr lib) name))))))
      (r7rs-import-library-filename-opt (cdr libs))))
) ;define

(define t11
  (timeit (lambda () (r7rs-import-library-filename-opt '((liii json)))) '() iterations))
(report "候选 先查缓存(已加载)" iterations t11)

;; 第四部分：拆解 import 展开的各环节，定位剩余开销
(newline)
(display "=== import 展开形态拆解 ===")
(newline)

(define t12
  (timeit (lambda ()
            (let ((sym (symbol (object->string '(liii json)))))
              (if (not (defined? sym))
                (format () "~A not loaded~%" sym)
                (symbol->value sym))))
          '() iterations))
(report "展开 else 分支(取库环境)" iterations t12)

(define t13
  (timeit (lambda () (varlet (curlet) json-env)) '() iterations))
(report "varlet 进当前环境" iterations t13)

(define t14
  (timeit (lambda ()
            (begin
              (r7rs-import-library-filename '((liii json)))
              (varlet (curlet) json-env)))
          '() iterations))
(report "手工展开(预取环境)" iterations t14)

(define t15
  (timeit (lambda ()
            (begin
              (r7rs-import-library-filename '((liii json)))
              (varlet (curlet)
                (let ((sym (symbol (object->string '(liii json)))))
                  (if (not (defined? sym))
                    (format () "~A not loaded~%" sym)
                    (symbol->value sym))))))
          '() iterations))
(report "手工完整展开(等价 import)" iterations t15)

;; 第五部分：验证 only/except/prefix/rename 装饰导入是否绕过 defined? 缓存
(newline)
(display "=== 装饰导入的缓存行为 ===")
(newline)

(display "(defined? 未剥离装饰的库名) => ")
(display (defined? (symbol (object->string '(only (liii json) json-read)))))
(newline)
(display "(defined? 剥离后的库名) => ")
(display (defined? (symbol (object->string '(liii json)))))
(newline)

(define t16
  (timeit (lambda () (import (only (liii json) json-read))) '() 1000))
(report "现状 热导入 (only (liii json) json-read)" 1000 t16)
