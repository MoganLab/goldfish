(import (liii check)
  (liii enum)
  (srfi srfi-128)
) ;import


(check-set-mode! 'report-failed)


(define pizza-descriptions
  '((margherita "tomato and mozzarella") (funghi "mushrooms") (bianca "ricotta and mozzarella") (chicago "deep-dish") (hawaiian "pineapple and ham"))
) ;define


(define pizza
  (make-enum-type pizza-descriptions)
) ;define


(define pizza-chicago
  (enum-name->enum pizza 'chicago)
) ;define


(define pizza-bianca
  (enum-name->enum pizza 'bianca)
) ;define


;; make-enum-comparator
;; 创建用于比较 enum 的 SRFI-128 比较器。
;;
;; 语法
;; ----
;; (make-enum-comparator enum-type)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; 返回值
;; ----
;; comparator
;; 用于同一 enum-type 的比较器对象。
;;
;; 注意
;; ----
;; 比较器支持顺序比较和哈希。
;;
;; 示例
;; ----
;; (comparator? (make-enum-comparator pizza)) => #t
;;
;; 错误处理
;; ----
;; 无。


(define pizza-comparator
  (make-enum-comparator pizza)
) ;define
(check (comparator? pizza-comparator)
  =>
  #t
) ;check
(check (comparator-ordered? pizza-comparator)
  =>
  #t
) ;check
(check (comparator-hashable? pizza-comparator)
  =>
  #t
) ;check
(check (=? pizza-comparator
         pizza-chicago
         (enum-name->enum pizza 'chicago)
       ) ;=?
  =>
  #t
) ;check
(check (=? pizza-comparator
         pizza-bianca
         pizza-chicago
       ) ;=?
  =>
  #f
) ;check
(check (<? pizza-comparator
         pizza-bianca
         pizza-chicago
       ) ;<?
  =>
  #t
) ;check
(check (<? pizza-comparator
         pizza-bianca
         pizza-bianca
       ) ;<?
  =>
  #f
) ;check
(check (>? pizza-comparator
         pizza-chicago
         pizza-bianca
       ) ;>?
  =>
  #t
) ;check
(check (<=? pizza-comparator
         pizza-bianca
         pizza-chicago
       ) ;<=?
  =>
  #t
) ;check
(check (>=? pizza-comparator
         pizza-chicago
         pizza-bianca
       ) ;>=?
  =>
  #t
) ;check


(check-report)
