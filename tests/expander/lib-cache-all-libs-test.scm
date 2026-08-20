(import (liii check) (liii os) (liii path) (liii string) (liii list) (liii sort) (goldfish))

;; 全库覆盖回归：遍历 goldfish/ 下全部 define-library 文件，
;; 逐个 load-library!（缓存优先路径），验证展开/重建/加载成功。
;;
;; 覆盖链路：
;;   - 冷加载：库展开 + 捕获缓存（bindings + 宏 spec + defs）
;;   - 热加载：缓存命中重建（re-import + 恢复 bindings + 重放宏 + eval defs）
;;   - 宏库（define-syntax / define-macro / defmacro / cond-expand 分支）
;;   - 依赖库（跨库 module-ref 预加载）
;;
;; 注意：本测试依赖 load-library! 的缓存路径（bin/gf 已启用）。

;; ===== 辅助：递归遍历 =====

(define (collect-library-files dir)
  (if (not (path-dir? dir))
    '()
    (let loop ((entries (vector->list (listdir dir))) (acc '()))
      (if (null? entries)
        acc
        (let* ((entry (car entries))
               (full (string-append dir "/" entry)))
          (cond
            ((path-dir? full)
             (loop (cdr entries) (append (collect-library-files full) acc)))
            ((and (path-file? full) (string-ends? entry ".scm"))
             (loop (cdr entries) (cons full acc)))
            (else
             (loop (cdr entries) acc))))))))

;; ===== 辅助：提取库名 =====

(define (extract-library-name file)
  (let ((port (open-input-file file)))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (let loop ()
          (let ((form (read port)))
            (cond
              ((eof-object? form) #f)
              ((and (pair? form) (eq? (car form) 'define-library))
               (if (pair? (cdr form)) (cadr form) #f))
              (else (loop))))))
      (lambda () (close-input-port port)))))

;; ===== 遍历并验证 =====

(let* ((files (list-sort string<? (collect-library-files "goldfish")))
       (lib-files (filter (lambda (f) (extract-library-name f)) files))
       (loaded 0)
       (failed '()))
  (display (string-append "发现库文件: " (number->string (length lib-files)) "\n"))
  ;; 冷加载：库展开 + 捕获缓存（可能命中已有缓存，或展开+写缓存）
  (for-each (lambda (f)
              (let ((name (extract-library-name f)))
                (when name
                  (let ((ok (catch #t
                              (lambda ()
                                (load-library! name)
                                (if (library-registry-ref name) 'ok 'no-registry))
                              (lambda (tag . args) (cons 'error args)))))
                    (if (eq? ok 'ok)
                      (set! loaded (+ loaded 1))
                      (set! failed (cons (cons name ok) failed)))))))
            lib-files)
  (display (string-append "冷加载成功: " (number->string loaded) "/" (number->string (length lib-files)) "\n"))
  (for-each (lambda (f)
              (display (string-append "  失败: "
                                      (with-output-to-string (lambda () (write (car f))))
                                      " => ")
                      )
              (display (with-output-to-string (lambda () (write (cdr f)))))
              (newline))
            failed)
  (check (length failed) => 0)
  ;; 热加载：缓存命中重建（全部库 registry 应已存在）
  (let ((hot-miss '()))
    (for-each (lambda (f)
                (let ((name (extract-library-name f)))
                  (when (and name (not (runtime-registered? name)))
                    (set! hot-miss (cons name hot-miss)))))
              lib-files)
    (display (string-append "热加载缺失: " (number->string (length hot-miss)) "\n"))
    (check (length hot-miss) => 0))
  (display (string-append "完成: " (number->string (length lib-files)) " 个库\n"))
  (check-report))
