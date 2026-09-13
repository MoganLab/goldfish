(import (liii check) (goldfish) (liii set))

;; define-library 缓存：import 组顺序回归。
;;
;; restore 按记录顺序重装 import view（后 import 优先），capture 必须
;; 按源码顺序存组。组顺序曾存反，warm 下 (liii set) 的 uses 变成
;; [(goldfish) ... srfi-113]，(goldfish) 的 primitive set-union 压掉
;; srfi-113 的 toplevel，同名调用展开成裸符号，求值命中宿主的 list 版
;; set-union，对 record-set 即 car-on-vector 崩溃。
;;
;; 两层断言：
;;   1. 记录断言（确定性）：磁盘记录的 import 组顺序 == 源码子句顺序。
;;   2. 行为断言：经 (liii set) 转发的 set-union 对 record-set 正确。

(define bundle-section (module-ref the-expander-library 'bundle-section))

;;; 磁盘记录文件（当前优化等级优先；只读缓存下可能不存在）。
(define (set-lib-gfo)
  (let ((base (string-append (gfo-dir) "/" (gfo-key "liii/set.scm"))))
    (let loop ((suffixes '("-o2" "" "-o1" "-o3")))
      (if (null? suffixes)
        #f
        (let ((f (string-append base (car suffixes) ".gfo")))
          (if (file-exists? f) f (loop (cdr suffixes))))))))

;;; 源码 define-library 的 import 子句组（源码顺序，datum）。
(define (source-import-groups path)
  (let ((forms (call-with-input-file path
                 (lambda (port)
                   (let loop ((acc '()))
                     (let ((d (read port)))
                       (if (eof-object? d)
                         (reverse acc)
                         (loop (cons d acc)))))))))
    (let find-lib ((fs forms))
      (if (null? fs)
        '()
        (let ((f (car fs)))
          (if (and (pair? f) (eq? (car f) 'define-library))
            (let collect ((cs (cddr f)) (acc '()))
              (if (null? cs)
                (reverse acc)
                (let ((c (car cs)))
                  (collect (cdr cs)
                           (if (and (pair? c) (eq? (car c) 'import))
                             (cons (cdr c) acc)
                             acc)))))
            (find-lib (cdr fs))))))))

;;; 记录形如 (name exports imports bindings macros defs [renames])。
(define (stored-import-groups gfo)
  (let* ((envelope (call-with-input-file gfo read))
         (payload (list-ref envelope 3))
         (recs (cdr (bundle-section payload 'libs))))
    (let find-rec ((rs recs))
      (if (null? rs)
        #f
        (if (equal? (car (car rs)) '(liii set))
          (caddr (car rs))
          (find-rec (cdr rs)))))))

(let ((gfo (set-lib-gfo)))
  ;; 只读缓存且尚无记录时跳过（库照常冷展开可用）。
  (when gfo
    (check (stored-import-groups gfo)
           => (source-import-groups "goldfish/liii/set.scm"))))

;;; 经 (liii set) 转发的 set-union：record-set 并集正确。
(check (set-size (set-union (set 1 2) (set 2 3))) => 3)

(check-report)
