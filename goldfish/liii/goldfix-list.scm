;;; Goldfix - 列表工具模块
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define-library (liii goldfix-list)
  (import (scheme base))

  ;; ---------- 导出接口 ----------
  (export list-max)
  (export list-min)
  (export find-first)
  (export list-set)

  (begin

    ;; 查找列表中某字段的最大值
    ;; 输入: lst - 列表
    ;;       accessor - 访问器函数 (lambda (item) value)
    ;; 输出: 最大值或 #f（如果列表为空）
    (define (list-max lst accessor)
      (let loop ((rest lst) (max-so-far #f))
        (if (null? rest)
          max-so-far
          (let ((val (accessor (car rest))))
            (loop (cdr rest)
                  ;; 防御性检查：确保 val 是数字
                  (if (and (number? val)
                           (or (not max-so-far) (> val max-so-far)))
                    val
                    max-so-far
                  ) ;if
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 查找列表中某字段的最小值
    ;; 输入: lst - 列表
    ;;       accessor - 访问器函数 (lambda (item) value)
    ;; 输出: 最小值或 #f（如果列表为空）
    (define (list-min lst accessor)
      (let loop ((rest lst) (min-so-far #f))
        (if (null? rest)
          min-so-far
          (let ((val (accessor (car rest))))
            (loop (cdr rest)
                  (if (or (not min-so-far) (< val min-so-far))
                    val
                    min-so-far
                  ) ;if
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 查找第一个匹配的元素
    ;; 输入: pred - 谓词函数
    ;;       lst - 列表
    ;; 输出: 第一个匹配元素或 #f
    (define (find-first pred lst)
      (let loop ((rest lst))
        (cond
          ((null? rest) #f)
          ((pred (car rest)) (car rest))
          (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 设置列表中指定位置的元素
    ;; 输入: lst - 列表
    ;;       idx - 索引位置（0-based）
    ;;       val - 新值
    ;; 输出: 新列表（原列表不受影响）
    (define (list-set lst idx val)
      (let loop ((i 0) (rest lst) (result '()))
        (cond ((null? rest) (reverse result))
              ((= i idx) (loop (+ i 1) (cdr rest) (cons val result)))
              (else (loop (+ i 1) (cdr rest) (cons (car rest) result)))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
