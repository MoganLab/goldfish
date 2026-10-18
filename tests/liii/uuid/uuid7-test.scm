(import (liii check)
        (liii uuid)
        (liii os)
        (liii sys)
        (liii hash-table))

(check-set-mode! 'report-failed)

;; uuid7
;; 生成一个 RFC 9562 标准 UUIDv7 时间有序唯一标识符。
;;
;; 语法
;; ----
;; (uuid7)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; string
;; 返回一个 36 字符的标准 UUID 字符串，格式为 xxxxxxxx-xxxx-7xxx-[89ab]xxx-xxxxxxxxxxxx。
;;
;; 注意
;; ----
;; 前 48 位包含毫秒级 Unix 时间戳，按时间生成具有单调递增性，对数据库索引与事件回溯友好。
;;
;; 示例
;; ----
;; (uuid7) => "018f3a2b-8a00-7abc-9def-0123456789ab"
;;
;; 错误处理
;; ------
;; 无异常抛出

;; 基础长度检验
(check (string-length (uuid7)) => 36)

;; 格式检验：8-4-4-4-12 分隔符结构
(define uuid-str (uuid7))
(check (char=? (string-ref uuid-str 8) #\-) => #t)
(check (char=? (string-ref uuid-str 13) #\-) => #t)
(check (char=? (string-ref uuid-str 18) #\-) => #t)
(check (char=? (string-ref uuid-str 23) #\-) => #t)

;; 版本号校验：第 15 个字符（索引 14）恒为 #\7
(check (char=? (string-ref uuid-str 14) #\7) => #t)

;; 变体位校验：第 20 个字符（索引 19）为 10xx 变体（8, 9, a, b）
(define (valid-uuid7-variant? u)
  (let ((ch (string-ref u 19)))
    (if (memv ch '(#\8 #\9 #\a #\b #\A #\B)) #t #f)))

(check (valid-uuid7-variant? (uuid7)) => #t)

;; 批量版本位与变体位校验
(define (check-uuid7-batch n)
  (let loop ((i 0))
    (if (= i n)
        #t
        (let ((u (uuid7)))
          (if (and (char=? (string-ref u 14) #\7)
                   (valid-uuid7-variant? u))
              (loop (+ i 1))
              #f)))))

(check (check-uuid7-batch 100) => #t)

;; 时间戳精度反解：前 48 位 Unix 毫秒时间戳与当前系统时钟 (os-time) 误差小于 5 秒
(let* ((now-sec (os-time))
       (u (uuid7))
       (hex-ts (string-append (substring u 0 8) (substring u 9 13)))
       (ts-ms (string->number hex-ts 16))
       (uuid-sec (/ ts-ms 1000.0))
       (diff (abs (- uuid-sec now-sec))))
  (check (< diff 5.0) => #t))

;; 单调递增性校验：引入微小延迟后后续生成的 UUID 字典序大于或等于先前生成的 UUID
(define (check-uuid7-monotonic n)
  (let loop ((i 0)
             (prev (uuid7)))
    (if (= i n)
        #t
        (begin
          (sleep 0.001)
          (let ((curr (uuid7)))
            (if (string<=? prev curr)
                (loop (+ i 1) curr)
                #f))))))

(check (check-uuid7-monotonic 50) => #t)

;; 高频连续调用零碰撞唯一性
(define (check-uuid7-uniqueness n)
  (let ((ht (make-hash-table)))
    (let loop ((i 0))
      (if (= i n)
          (= (hash-table-size ht) n)
          (let ((u (uuid7)))
            (if (hash-table-ref/default ht u #f)
                #f
                (begin
                  (hash-table-set! ht u #t)
                  (loop (+ i 1)))))))))

(check (check-uuid7-uniqueness 1000) => #t)

(check-report)
