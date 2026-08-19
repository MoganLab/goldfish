;; (liii json) 与 (liii njson) 性能对比
;; 语料：JSONTestSuite test_parsing 的 y_ 用例（与 demo/json_spec.scm 同源）
;; 运行方式: bin/gf bench/json-vs-njson-perf.scm [suite-dir]

(import (liii base)
  (liii list)
  (liii sort)
  (liii path)
  (liii json)
  (liii njson)
  (liii timeit)
  (liii unicode)
  (liii subprocess)
  (liii string-cursor)
  (scheme process-context)
) ;import

(define clone-dir (path-join (path-home) "git" "JSONTestSuite"))

(unless (path-exists? clone-dir)
  (let ((code (run (list 'git
                     "clone"
                     "--depth"
                     "1"
                     "https://github.com/nst/JSONTestSuite.git"
                     (path->string clone-dir)
                   ) ;list
              ) ;run
        ) ;code
       ) ;
    (unless (zero? code)
      (error 'json-vs-njson "failed to clone JSONTestSuite")
    ) ;unless
  ) ;let
) ;unless

(define suite-dir
  (if (> (length (command-line)) 2)
    (cadr (command-line))
    (path->string (path-join clone-dir "test_parsing"))
  ) ;if
) ;define

;; 收集 y_ 用例文本，且要求两种实现都能解析成功（保证对比同一语料）

(define pairs
  (filter pair?
    (map (lambda (f)
           (catch #t
             (lambda ()
               (let ((text (utf8->string (path-read-bytes (path-join suite-dir f)))))
                 (string->json text)
                 (string->njson text)
                 (cons f text)
               ) ;let
             ) ;lambda
             (lambda (type info) #f)
           ) ;catch
         ) ;lambda
      (filter (lambda (f) (string-prefix? "y_" f))
        (list-sort string<? (vector->list (path-list suite-dir)))
      ) ;filter
    ) ;map
  ) ;filter
) ;define

(define texts (map cdr pairs))

(display "语料文件数（双方均可解析的 y_ 用例）: ")
(display (length texts))
(newline)

;; 预解析出两边的对象，用于序列化对比

(define ljson-objs (map string->json texts))

(define njson-objs (map string->njson texts))

;; 正确性 sanity check：njson->json(json) 与 json 应 equal?
(let loop
  ((fs (map car pairs)) (js ljson-objs) (ns njson-objs) (bad '()))
  (cond ((null? js)
         (display "njson->json 一致性检查: ")
         (display (if (null? bad)
                    "全部一致"
                    (string-append "不一致 " (number->string (length bad)) " 例")
                  ) ;if
         ) ;display
         (newline)
         (for-each (lambda (f) (display "  不一致: ") (display f) (newline))
           (reverse bad)
         ) ;for-each
        ) ;
        (else (let ((ok (catch #t
                          (lambda () (equal? (car js) (njson->json (car ns))))
                          (lambda (type info) #f)
                        ) ;catch
                    ) ;ok
                   ) ;
                (loop (cdr fs) (cdr js) (cdr ns) (if ok bad (cons (car fs) bad)))
              ) ;let
        ) ;else
  ) ;cond
) ;let

(define (parse-json)
  (for-each string->json texts)
) ;define

(define (parse-njson)
  (for-each string->njson texts)
) ;define

(define (emit-json)
  (for-each json->string ljson-objs)
) ;define

(define (emit-njson)
  (for-each njson->string njson-objs)
) ;define

;; warmup
(parse-json)
(parse-njson)
(emit-json)
(emit-njson)

(define iter 50)

(define (report title iter-count time-val)
  (display "[")
  (display title)
  (display "] iterations=")
  (display iter-count)
  (display " time=")
  (display time-val)
  (display "s")
  (newline)
) ;define

(display "=== (liii json) vs (liii njson) 整语料对比 ===")
(newline)
(newline)
(let ((tj (timeit parse-json '() iter)) (tn (timeit parse-njson '() iter)))
  (report "解析/json " iter tj)
  (report "解析/njson" iter tn)
  (display "解析比 json/njson = ")
  (display (/ tj tn))
  (newline)
) ;let
(newline)
(let ((tj (timeit emit-json '() iter)) (tn (timeit emit-njson '() iter)))
  (report "序列化/json " iter tj)
  (report "序列化/njson" iter tn)
  (display "序列化比 json/njson = ")
  (display (/ tj tn))
  (newline)
) ;let

;; 大 payload 对比（构造 200 键对象 / 200 元素数组，语法同 bench/string-to-json-perf.scm）

(define (build-json-string n)
  (let ((out (open-output-string)))
    (display "{" out)
    (do ((i 0 (+ i 1)))
      ((= i n))
      (when (> i 0)
        (display "," out)
      ) ;when
      (display "\"k" out)
      (display i out)
      (display "\":" out)
      (display i out)
    ) ;do
    (display "}" out)
    (get-output-string out)
  ) ;let
) ;define

(define (build-array-string n)
  (let ((out (open-output-string)))
    (display "[" out)
    (do ((i 0 (+ i 1)))
      ((= i n))
      (when (> i 0)
        (display "," out)
      ) ;when
      (display i out)
    ) ;do
    (display "]" out)
    (get-output-string out)
  ) ;let
) ;define

(define big-obj-str (build-json-string 200))

(define big-arr-str (build-array-string 200))

(define big-obj-json (string->json big-obj-str))

(define big-arr-json (string->json big-arr-str))

(define big-obj-njson (string->njson big-obj-str))

(define big-arr-njson (string->njson big-arr-str))

(define iter2 200)

(newline)
(display "=== 大 payload 对比（200 键对象 / 200 元素数组） ===")
(newline)
(newline)
(let ((tj (timeit (lambda () (string->json big-obj-str)) '() iter2))
      (tn (timeit (lambda () (string->njson big-obj-str)) '() iter2))
     ) ;
  (report "大对象解析/json " iter2 tj)
  (report "大对象解析/njson" iter2 tn)
  (display "解析比 json/njson = ")
  (display (/ tj tn))
  (newline)
) ;let
(let ((tj (timeit (lambda () (string->json big-arr-str)) '() iter2))
      (tn (timeit (lambda () (string->njson big-arr-str)) '() iter2))
     ) ;
  (report "大数组解析/json " iter2 tj)
  (report "大数组解析/njson" iter2 tn)
  (display "解析比 json/njson = ")
  (display (/ tj tn))
  (newline)
) ;let
(let ((tj (timeit (lambda () (json->string big-obj-json)) '() iter2))
      (tn (timeit (lambda () (njson->string big-obj-njson)) '() iter2))
     ) ;
  (report "大对象序列化/json " iter2 tj)
  (report "大对象序列化/njson" iter2 tn)
  (display "序列化比 json/njson = ")
  (display (/ tj tn))
  (newline)
) ;let
(let ((tj (timeit (lambda () (json->string big-arr-json)) '() iter2))
      (tn (timeit (lambda () (njson->string big-arr-njson)) '() iter2))
     ) ;
  (report "大数组序列化/json " iter2 tj)
  (report "大数组序列化/njson" iter2 tn)
  (display "序列化比 json/njson = ")
  (display (/ tj tn))
  (newline)
) ;let
