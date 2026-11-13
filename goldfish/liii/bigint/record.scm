;;; liii/bigint/record.scm - bigint 记录类型基础定义
;;;
;;; 参考 Java BigInteger 设计：
;;; - signum: 符号 (-1, 0, 1)
;;; - mag: magnitude 数组，大端序存储，每个元素是 0 到 BASE-1
;;; - 零用 signum=0 和 mag='() 表示
;;;
;;; Copyright (c) 2024 Liii Network
;;; All Rights Reserved

(define-library (liii bigint record)
  (import (scheme base) (scheme cxr) (liii error) (liii list))

  (export make-bigint bigint? bigint-signum bigint-mag bigint-normalize
    strip-leading-zeros BIGINT-BASE BIGINT-BITS
  ) ;export

  (begin
    ;;; ---------- 常量定义 ----------
    ;; 基数：使用 2^32，与 Java 的 int[] 一致
    ;; 每个 "digit" 是一个 32 位无符号整数
    (define BIGINT-BASE 4294967296)
    (define BIGINT-BITS 32)

    ;;; ---------- 记录类型定义 ----------
    ;; 参考 Java BigInteger 的内部结构：
    ;; signum: -1 表示负数, 0 表示零, 1 表示正数
    ;; mag: magnitude 数组，大端序（最高有效位在索引 0）
    ;; 注意：mag 必须是 "最小化" 的，即最高位不为零（零除外）
    (define-record-type <bigint>
      (make-bigint-raw signum mag)
      bigint?
      (signum bigint-signum)
      (mag bigint-mag)
    ) ;define-record-type

    ;;; ---------- 内部辅助函数 ----------
    ;; 验证 mag 数组是否合法
    (define (valid-mag? mag)
      (and (list? mag)
        (every (lambda (d) (and (integer? d) (>= d 0) (< d BIGINT-BASE))) mag)
      ) ;and
    ) ;define

    ;; 检查 mag 是否是规范化的（最显著位非零，或为空表示零）
    (define (normalized-mag? mag)
      (or (null? mag) (and (> (length mag) 0) (not (= (car mag) 0))))
    ) ;define

    ;; 去除 mag 的前导零（大端序）
    (define (strip-leading-zeros mag)
      (cond ((null? mag) '())
            ((and (= (length mag) 1) (= (car mag) 0)) '())
            ((= (car mag) 0) (strip-leading-zeros (cdr mag)))
            (else mag)
      ) ;cond
    ) ;define

    ;;; ---------- 构造函数 ----------
    ;; 创建规范化的 bigint
    ;; signum: -1, 0, 或 1
    ;; mag: magnitude 列表（会被规范化）
    (define (make-bigint signum mag)
      (cond
        ;; 符号必须是 -1, 0, 或 1
        ((not (memv signum '(-1 0 1)))
         (value-error "make-bigint in (liii bigint record): invalid signum, must be -1, 0, or 1"
           signum
         ) ;value-error
        ) ;

        ;; mag 必须合法
        ((not (valid-mag? mag))
         (value-error "make-bigint in (liii bigint record): invalid mag, elements must be 0 <= d < BASE"
           mag
         ) ;value-error
        ) ;

        ;; 符号为 0 时必须是零（空 mag 或全零）
        ((= signum 0)
         (if
           (or (null? mag)
             (and (= (length mag) 1) (= (car mag) 0))
             (every (lambda (d) (= d 0)) mag)
           ) ;or
           (make-bigint-raw 0 '())
           (value-error "make-bigint in (liii bigint record): signum is 0 but mag is non-zero"
             mag
           ) ;value-error
         ) ;if
        ) ;

        ;; 其他情况：规范化 mag
        (else
          (let ((norm-mag (strip-leading-zeros mag)))
            (if (null? norm-mag) (make-bigint-raw 0 '()) (make-bigint-raw signum norm-mag))
          ) ;let
        ) ;else
      ) ;cond
    ) ;define

    ;; 规范化现有的 bigint（去除前导零，调整符号）
    (define (bigint-normalize n)
      (if (not (bigint? n))
        (type-error "bigint-normalize in (liii bigint record): not a bigint" n)
        (let ((signum (bigint-signum n)) (mag (bigint-mag n)))
          (make-bigint signum mag)
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
