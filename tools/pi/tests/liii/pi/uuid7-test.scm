(import (liii check)
        (liii uuid)
        (scheme time))

(check-set-mode! 'report-failed)

;; uuid7
;; 生成一个 RFC 9562 标准 UUIDv7 时间有序唯一标识符。
(let ((id (uuid7)))
  (check-true (string? id))
  (check (string-length id) => 36)
  ;; 8-4-4-4-12 分隔符结构
  (check (char=? (string-ref id 8) #\-) => #t)
  (check (char=? (string-ref id 13) #\-) => #t)
  (check (char=? (string-ref id 18) #\-) => #t)
  (check (char=? (string-ref id 23) #\-) => #t)
  ;; 版本位（索引 14）恒为 #\7
  (check (char=? (string-ref id 14) #\7) => #t)
  ;; 变体位（索引 19）为 10xx 变体 (8, 9, a, b)
  (check-true (and (memv (string-ref id 19) '(#\8 #\9 #\a #\b #\A #\B)) #t))
  ;; 时间戳与当前系统时间合理吻合
  (let* ((hex-ts (string-append (substring id 0 8) (substring id 9 13)))
         (ts-ms (string->number hex-ts 16))
         (uuid-sec (/ ts-ms 1000.0))
         (now-sec (current-second))
         (diff (abs (- uuid-sec now-sec))))
    (check-true (< diff 120.0))))

(check-report)
