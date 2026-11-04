;; e2e-string-to-json-memory-leak
;; 回归测试：issue 1311，string->json 循环解析大输入时垃圾堆积导致 RSS 线性增长且 (gc) 无法回收。
;;
;; 语法
;; ----
;; bin/gf tests/liii/json/e2e-string-to-json-memory-leak-test.scm
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; -----
;; unspecified
;; 通过 check 输出三个场景的 RSS 增量断言结果。
;;
;; 说明
;; ----
;; 修复：f_string_to_json 按累计输入字节数周期性触发 GC（src/liii_json.cpp）。
;; 场景 1：1401 字节扁平数组 x2000，修复前 RSS 增长约 16.5MB。
;; 场景 2：约 3KB 嵌套对象 x2000，修复前 RSS 增长约 79MB（issue 中 7KB 文本达 900MB）。
;; 场景 3：read-json 端口解析同样受保护。

(import (liii json)
  (liii psutil)
  (liii check)
  (liii list)
  (scheme base)
  (scheme process-context)
  (scheme write)
) ;import

(check-set-mode! 'report)

(define (rss)
  (memory-info-resident-set-memory-size (process-memory-info))
) ;define

;; 1401 字节扁平数组 [1,1,...,1]

(define text-array
  (string-append "["
    (apply string-append (map (lambda (i) (if (zero? i) "1" ",1")) (iota 700)))
    "]"
  ) ;string-append
) ;define

;; ~3KB 嵌套对象（issue 中最严重场景的缩小版）

(define (mk-entry i)
  (string-append "{\"id\":"
    (number->string i)
    ",\"name\":\"item-name-with-some-length-"
    (number->string i)
    "\""
    ",\"tags\":[\"alpha\",\"beta\",\"gamma\"]"
    ",\"meta\":{\"created\":\"2024-01-01\",\"score\":"
    (number->string (modulo i 100))
    ",\"active\":true,\"note\":\"a reasonably long note field for padding purposes\"}}"
  ) ;string-append
) ;define

(define text-nested
  (apply string-append
    (cons "["
      (append
        (map
          (lambda (i) (if (zero? i) (mk-entry i) (string-append "," (mk-entry i))))
          (iota 16)
        ) ;map
        (list "]")
      ) ;append
    ) ;cons
  ) ;apply
) ;define

;; 场景1: 扁平数组 x2000
;; 修复前: RSS 增长约 16.5MB；修复后: 周期 GC 保证增长远小于此
(gc)

(define r0 (rss))
(do ((i 0 (+ i 1)))
  ((= i 2000))
  (string->json text-array)
) ;do
(gc)

(define delta-array (- (rss) r0))
(display "rss-delta array(1401B) x2000: ")
(display delta-array)
(newline)
(check (< delta-array (* 8 1024 1024)) => #t)

;; 场景2: 嵌套对象 x2000
;; 修复前: RSS 增长约 79MB（issue 中 7KB 文本达 900MB）；修复后应远小于此
(gc)
(set! r0 (rss))
(do ((i 0 (+ i 1)))
  ((= i 2000))
  (string->json text-nested)
) ;do
(gc)

(define delta-nested (- (rss) r0))
(display "rss-delta nested(")
(display (string-length text-nested))
(display "B) x2000: ")
(display delta-nested)
(newline)
(check (< delta-nested (* 32 1024 1024)) => #t)

;; 场景3: read-json 端口解析同样受保护
(gc)
(set! r0 (rss))
(do ((i 0 (+ i 1)))
  ((= i 2000))
  (call-with-input-string text-nested read-json)
) ;do
(gc)

(define delta-port (- (rss) r0))
(display "rss-delta read-json x2000: ")
(display delta-port)
(newline)
(check (< delta-port (* 32 1024 1024)) => #t)

(check-report)
