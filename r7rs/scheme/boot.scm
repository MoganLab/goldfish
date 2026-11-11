(unless (defined? 'texmacs-module)
  (set! (*s7* 'scheme-version) 'r7rs)
) ;unless

;; [1147] 堆增长策略调优：
;; 1. gc-resize-heap-fraction 默认 0.8，即堆占用超过 20% 就翻倍，且堆只增
;;    不减；大量库 import 的瞬时峰值会把堆推到数倍于活跃集的大小，RSS 虚
;;    高且不归还 OS。调低为 0.15（占用超过 85% 才扩容），实测 gf pi 的
;;    252 库加载链主会话 RSS 60MB -> 37MB、go worker 增量 47MB -> 21MB。
;; 2. gc-resize-heap-by-4-fraction 默认 0.67，空闲紧张时一次扩容 4 倍，
;;    峰值越界时堆会从 12MB 直接跳到 49MB；调低为 0.01 后扩容实际上总是
;;    翻倍，最坏 24MB，行为可预期（s7 要求该值大于 0，无法置 0）。
;; go worker 会话同样加载本文件，主/worker 会话统一生效。
(set! (*s7* 'gc-resize-heap-fraction) 0.15)
(set! (*s7* 'gc-resize-heap-by-4-fraction) 0.01)

(define (file-exists? path)
  (if (string? path)
    (if (not (g_access path 0))
      #f
      (if (g_access path 1)
        #t
        (error 'permission-error (string-append "No permission: " path))
      ) ;if
    ) ;if
    (error 'type-error "(file-exists? path): path should be string")
  ) ;if
) ;define

(define (delete-file path)
  (if (not (string? path))
    (error 'type-error "(delete-file path): path should be string")
    (if (not (file-exists? path))
      (error 'read-error (string-append path " does not exist"))
      (g_delete-file path)
    ) ;if
  ) ;if
) ;define

;; UTF-8 码点编解码原语
;;
;; (scheme ...) 系列库只承载 R7RS-small 定义的内容，而 UTF-8 码点编解码属于语言
;; 底层的字节设施，因此放在自举入口这一层，供 (scheme char) 等 R7RS 库使用，
;; 避免 R7RS 库反向依赖标准库 (liii unicode)。
;;
;; 实现只使用 s7 内建的 rootlet 原语（logior/logand/ash/byte-vector 等），
;; 不依赖任何 (liii ...) 或 (srfi ...) 库。
;;
;; 注意：本文件的符号位于 rootlet，(scheme ...) 库可以直接使用（与
;; syntax-rules.scm 提供的 syntax-rules / define-syntax 同一惯例）。

;; R7RS 6.11 Exceptions: error-object
(define-record-type <error-object>
  (%make-error-object kind message irritants)
  error-object?
  (kind %error-object-kind)
  (message error-object-message)
  (irritants error-object-irritants)
) ;define-record-type

(define (codepoint->utf8 codepoint)
  (unless (integer? codepoint)
    (error 'type-error "codepoint->utf8: expected integer, got" codepoint)
  ) ;unless

  (when (or (< codepoint 0) (> codepoint 1114111))
    (error 'value-error "codepoint->utf8: codepoint out of Unicode range" codepoint)
  ) ;when

  (cond ((<= codepoint 127) (byte-vector codepoint))

        ((<= codepoint 2047)
         (let ((byte1 (logior 192 (logand (ash codepoint -6) 31)))
               (byte2 (logior 128 (logand codepoint 63)))
              ) ;
           (byte-vector byte1 byte2)
         ) ;let
        ) ;

        ((<= codepoint 65535)
         (let ((byte1 (logior 224 (logand (ash codepoint -12) 15)))
               (byte2 (logior 128 (logand (ash codepoint -6) 63)))
               (byte3 (logior 128 (logand codepoint 63)))
              ) ;
           (byte-vector byte1 byte2 byte3)
         ) ;let
        ) ;

        (else
          (let ((byte1 (logior 240 (logand (ash codepoint -18) 7)))
                (byte2 (logior 128 (logand (ash codepoint -12) 63)))
                (byte3 (logior 128 (logand (ash codepoint -6) 63)))
                (byte4 (logior 128 (logand codepoint 63)))
               ) ;
            (byte-vector byte1 byte2 byte3 byte4)
          ) ;let
        ) ;else
  ) ;cond
) ;define

(define (utf8->codepoint-at bv start)
  (unless (byte-vector? bv)
    (error 'type-error "utf8->codepoint-at: expected bytevector")
  ) ;unless

  (let ((len (length bv)))
    (when (>= start len)
      (error 'value-error "utf8->codepoint-at: start index past end of bytevector")
    ) ;when

    (let ((first-byte (byte-vector-ref bv start)))
      (cond ((<= first-byte 127) first-byte)

            ((<= 194 first-byte 223)
             (when (> (+ start 2) len)
               (error 'value-error "utf8->codepoint-at: incomplete 2-byte sequence")
             ) ;when
             (let ((byte2 (byte-vector-ref bv (+ start 1))))
               (unless (<= 128 byte2 191)
                 (error 'value-error "utf8->codepoint-at: invalid continuation byte")
               ) ;unless
               (logior (ash (logand first-byte 31) 6) (logand byte2 63))
             ) ;let
            ) ;

            ((<= 224 first-byte 239)
             (when (> (+ start 3) len)
               (error 'value-error "utf8->codepoint-at: incomplete 3-byte sequence")
             ) ;when
             (let ((byte2 (byte-vector-ref bv (+ start 1)))
                   (byte3 (byte-vector-ref bv (+ start 2)))
                  ) ;
               (unless (and (<= 128 byte2 191) (<= 128 byte3 191))
                 (error 'value-error "utf8->codepoint-at: invalid continuation byte")
               ) ;unless
               (let ((codepoint (logior (ash (logand first-byte 15) 12)
                                  (ash (logand byte2 63) 6)
                                  (logand byte3 63)
                                ) ;logior
                     ) ;codepoint
                    ) ;
                 (when (or (<= 55296 codepoint 57343)
                         (and (= first-byte 224) (< codepoint 2048))
                         (and (= first-byte 237) (>= codepoint 55296))
                       ) ;or
                   (error 'value-error "utf8->codepoint-at: invalid codepoint")
                 ) ;when
                 codepoint
               ) ;let
             ) ;let
            ) ;

            ((<= 240 first-byte 244)
             (when (> (+ start 4) len)
               (error 'value-error "utf8->codepoint-at: incomplete 4-byte sequence")
             ) ;when
             (let ((byte2 (byte-vector-ref bv (+ start 1)))
                   (byte3 (byte-vector-ref bv (+ start 2)))
                   (byte4 (byte-vector-ref bv (+ start 3)))
                  ) ;
               (unless (and (<= 128 byte2 191) (<= 128 byte3 191) (<= 128 byte4 191))
                 (error 'value-error "utf8->codepoint-at: invalid continuation byte")
               ) ;unless
               (let ((codepoint (logior (ash (logand first-byte 7) 18)
                                  (ash (logand byte2 63) 12)
                                  (ash (logand byte3 63) 6)
                                  (logand byte4 63)
                                ) ;logior
                     ) ;codepoint
                    ) ;
                 (when (or (< codepoint 65536)
                         (> codepoint 1114111)
                         (and (= first-byte 240) (< codepoint 65536))
                         (and (= first-byte 244) (> codepoint 1114111))
                       ) ;or
                   (error 'value-error "utf8->codepoint-at: invalid codepoint")
                 ) ;when
                 codepoint
               ) ;let
             ) ;let
            ) ;

            (else (error 'value-error "utf8->codepoint-at: invalid UTF-8 sequence"))
      ) ;cond
    ) ;let
  ) ;let
) ;define

(define (utf8-string . chars)
  ;; 由码点序列构造 UTF-8 字符串；等价于 (liii unicode) 的同名函数，
  ;; 实现下移至自举层，使 (scheme ...) 库不必依赖标准库。
  (let* ((parts
           (map
             (lambda (c)
               (if (char? c)
                 (codepoint->utf8 (char->integer c))
                 (error 'type-error "utf8-string: expected char" c)
               ) ;if
             ) ;lambda
             chars
           ) ;map
         ) ;parts
         (total
           (let loop
             ((rest parts) (n 0))
             (if (null? rest) n (loop (cdr rest) (+ n (length (car rest)))))
           ) ;let
         ) ;total
         (out (make-string total))
         (idx 0)
        ) ;
    (let fill
      ((rest parts))
      (if (null? rest)
        out
        (let ((bv (car rest)))
          (let inner
            ((i 0))
            (if (>= i (length bv))
              (fill (cdr rest))
              (begin
                (string-set! out idx (integer->char (byte-vector-ref bv i)))
                (set! idx (+ idx 1))
                (inner (+ i 1))
              ) ;begin
            ) ;if
          ) ;let
        ) ;let
      ) ;if
    ) ;let
  ) ;let*
) ;define

(load "scheme/syntax-rules.scm")
