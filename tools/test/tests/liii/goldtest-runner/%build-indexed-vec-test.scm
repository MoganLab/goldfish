;; %build-indexed-vec
;; 构造 (idx file) 索引化任务向量
;;
;; 语法
;; ----
;; (%build-indexed-vec test-files)
;;
;; 参数
;; ----
;; test-files : list?，测试文件路径列表
;;
;; 返回值
;; ------
;; vector?，元素为 (list idx file)，idx 从 0 起

(import (scheme base) (liii check) (liii goldtest-runner))

(check-set-mode! 'report-failed)

;; ===== 场景1: 空列表 =====
(check (%build-indexed-vec '()) => '#())

;; ===== 场景2: 逐元素索引 =====
(check (%build-indexed-vec '("a.scm" "b.scm" "c.scm"))
  => '#((0 "a.scm") (1 "b.scm") (2 "c.scm")))

;; ===== 场景3: 单元素 =====
(check (%build-indexed-vec '("only.scm")) => '#((0 "only.scm")))

;; ===== 场景4: 是过程 =====
(check-true (procedure? %build-indexed-vec))

(check-report)
