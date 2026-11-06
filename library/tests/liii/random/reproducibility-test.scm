;; random-reproducibility
;; 验证保存与恢复随机源状态可以重现相同的随机数序列。
;;
;; 语法
;; ----
;; (bin/gf test tests/liii/random/reproducibility-test.scm)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. 保存状态 -> 生成随机数 -> 恢复状态 -> 生成相同随机数。
;; 2. 用相同索引做伪随机化的两个随机源产生相同序列。
;; 3. 各随机源函数的单元测试见 tests/srfi/srfi-27/ 目录。

(import (liii random) (liii check))

(check-set-mode! 'report-failed)

;; 保存状态 -> 生成随机数 -> 恢复状态 -> 生成相同随机数
(let ((s (make-random-source)))
  (let ((saved-state (random-source-state-ref s)))
    (let ((rand-int (random-source-make-integers s)))
      (let ((r1 (rand-int 100)) (r2 (rand-int 100)) (r3 (rand-int 100)))
        (random-source-state-set! s saved-state)
        (let ((rand-int2 (random-source-make-integers s)))
          (let ((r1b (rand-int2 100)) (r2b (rand-int2 100)) (r3b (rand-int2 100)))
            (check (= r1 r1b) => #t)
            (check (= r2 r2b) => #t)
            (check (= r3 r3b) => #t)
          ) ;let
        ) ;let
      ) ;let
    ) ;let
  ) ;let
) ;let

;; 相同索引伪随机化的两个随机源产生相同序列
(let ((s1 (make-random-source)) (s2 (make-random-source)))
  (random-source-pseudo-randomize! s1 5 10)
  (random-source-pseudo-randomize! s2 5 10)
  (let ((rand1 (random-source-make-integers s1))
        (rand2 (random-source-make-integers s2))
       ) ;
    (let ((r1 (rand1 1000)) (r2 (rand2 1000)))
      (check (= r1 r2) => #t)
    ) ;let
  ) ;let
) ;let

(check-report)
