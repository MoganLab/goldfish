;;; Goldfix - 列表工具模块
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define-library (liii goldfix-list)
  (import (scheme base))
  (import (only (liii list) fold))

  ;; ---------- 导出接口 ----------
  (export list-max)
  (export list-min)
  (export list-set)

  (begin

    ;; 查找列表中某字段的最大值
    ;; 输入: lst - 列表
    ;;       accessor - 访问器函数 (lambda (item) value)
    ;; 输出: 最大值或 #f（如果列表为空）
    (define (list-max lst accessor)
      (fold (lambda (item max-so-far)
              (let ((val (accessor item)))
                (if (and (number? val)
                         (or (not max-so-far) (> val max-so-far)))
                  val
                  max-so-far
                ) ;if
              ) ;let
            ) ;lambda
            #f
            lst
      ) ;fold
    ) ;define

    ;; 查找列表中某字段的最小值
    ;; 输入: lst - 列表
    ;;       accessor - 访问器函数 (lambda (item) value)
    ;; 输出: 最小值或 #f（如果列表为空）
    (define (list-min lst accessor)
      (fold (lambda (item min-so-far)
              (let ((val (accessor item)))
                (if (or (not min-so-far) (< val min-so-far))
                  val
                  min-so-far
                ) ;if
              ) ;let
            ) ;lambda
            #f
            lst
      ) ;fold
    ) ;define

    ;; 设置列表中指定位置的元素
    ;; 输入: lst - 列表
    ;;       idx - 索引位置（0-based）
    ;;       val - 新值
    ;; 输出: 新列表（原列表不受影响）
    (define (list-set lst idx val)
      (let* ((result (list-copy lst))
             (target-idx (and (integer? idx) (exact idx))))
        (if (and target-idx
                 (>= target-idx 0)
                 (< target-idx (length result)))
          (begin
            (list-set! result target-idx val)
            result
          ) ;begin
          result
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
