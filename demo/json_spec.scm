;; 针对 JSONTestSuite 的 string->json / json->string 完整测试
;;
;; 用法：bin/gf demo/json_spec.scm [suite-dir]
;; 首次运行会自动将 JSONTestSuite 克隆到 ~/git/JSONTestSuite（已存在则跳过）；
;; suite-dir 默认为 ~/git/JSONTestSuite/test_parsing
;;
;; 分类规则（JSONTestSuite 约定）：
;;   y_ 前缀：应当解析成功
;;   n_ 前缀：应当解析失败（抛错）
;;   i_ 前缀：实现自定义，仅统计不判定
;;
;; 注意：goldfish 的 guard 在 body 正常返回时也会落入 else 子句，故用 catch

(import (liii base)
  (liii list)
  (liii sort)
  (liii path)
  (liii json)
  (liii unicode)
  (liii subprocess)
  (liii string-cursor)
  (scheme process-context)
) ;import

;; 第一步：确保 ~/git/JSONTestSuite 存在，不存在则克隆

(define clone-dir (path-join (path-home) "git" "JSONTestSuite"))

(unless (path-exists? clone-dir)
  (display "cloning JSONTestSuite into ")
  (display (path->string clone-dir))
  (newline)
  (let ((code (run (list 'git
                     "clone"
                     "--depth"
                     "1"
                     "https://github.com/nst/JSONTestSuite.git"
                     (path->string clone-dir)
                   ) ;list
              ) ;run
        ) ;code
       ) ;
    (unless (zero? code)
      (error 'json-spec "failed to clone JSONTestSuite")
    ) ;unless
  ) ;let
) ;unless

(define suite-dir
  (if (> (length (command-line)) 2)
    (cadr (command-line))
    (path->string (path-join clone-dir "test_parsing"))
  ) ;if
) ;define

(define (classify name)
  (cond ((string-prefix? "y_" name) 'yes)
        ((string-prefix? "n_" name) 'no)
        ((string-prefix? "i_" name) 'impl)
        (else 'other)
  ) ;cond
) ;define

;; 返回 'pass 'fail 'error 之一
;; 返回 'pass 'fail 之一

(define (try-parse text expected)
  (catch #t
    (lambda ()
      (let ((r (string->json text)))
        (if (eof-object? r)
          (if (eq? expected 'no) 'pass 'fail)
          (if (eq? expected 'no) 'fail 'pass)
        ) ;if
      ) ;let
    ) ;lambda
    (lambda (type info) (if (eq? expected 'no) 'pass 'fail))
  ) ;catch
) ;define

(define (try-one fname expected)
  (let ((text (catch #t
                (lambda ()
                  ;; 用字节读取再转字符串，避免文本读取在 NUL 字节处截断
                  (utf8->string (path-read-bytes (path-join suite-dir fname)))
                ) ;lambda
                (lambda (type info) "")
              ) ;catch
        ) ;text
       ) ;
    (try-parse text expected)
  ) ;let
) ;define

(define yes-pass 0)

(define yes-fail 0)

(define no-pass 0)

(define no-fail 0)

(define impl-count 0)

(define failures '())

(define json-files (list-sort string<? (vector->list (path-list suite-dir))))

(define stats '((yes 0 . 0) (no 0 . 0) (impl 0 . 0)))

(define (bump-kind kind result)
  (let ((cell (assq kind stats)))
    (if result
      (set-car! (cdr cell) (+ 1 (cadr cell)))
      (set-cdr! (cdr cell) (+ 1 (cddr cell)))
    ) ;if
  ) ;let
) ;define

(define failures '())

(for-each (lambda (f)
            (let ((kind (classify f)))
              (cond ((eq? kind 'yes)
                     (let ((r (try-one f 'yes)))
                       (if (eq? r 'pass)
                         (set! yes-pass (+ yes-pass 1))
                         (begin
                           (set! yes-fail (+ yes-fail 1))
                           (set! failures (cons (string-append f " => " (symbol->string r)) failures))
                         ) ;begin
                       ) ;if
                     ) ;let
                    ) ;
                    ((eq? kind 'no)
                     (let ((r (try-one f 'no)))
                       (if (eq? r 'pass)
                         (set! no-pass (+ no-pass 1))
                         (begin
                           (set! no-fail (+ no-fail 1))
                           (set! failures (cons (string-append f " => " (symbol->string r)) failures))
                         ) ;begin
                       ) ;if
                     ) ;let
                    ) ;
                    (else (set! impl-count (+ impl-count 1)))
              ) ;cond
            ) ;let
          ) ;lambda
  json-files
) ;for-each

;; round-trip：y_ 用例解析成功后，json->string 再 string->json 应与原解析结果 equal?

(define rt-pass 0)

(define rt-fail 0)

(define rt-failures '())
(for-each (lambda (f)
            (catch #t
              (lambda ()
                (let* ((text (utf8->string (path-read-bytes (path-join suite-dir f))))
                       (j1 (string->json text))
                      ) ;
                  (if (eof-object? j1)
                    (set! rt-pass (+ rt-pass 1))
                    (let* ((s (json->string j1)) (j2 (string->json s)))
                      (if (equal? j1 j2)
                        (set! rt-pass (+ rt-pass 1))
                        (begin
                          (set! rt-fail (+ rt-fail 1))
                          (set! rt-failures (cons f rt-failures))
                        ) ;begin
                      ) ;if
                    ) ;let*
                  ) ;if
                ) ;let*
              ) ;lambda
              (lambda (type info)
                (set! rt-fail (+ rt-fail 1))
                (set! rt-failures
                  (cons (string-append f " (" (symbol->string type) ")") rt-failures)
                ) ;set!
              ) ;lambda
            ) ;catch
          ) ;lambda
  (filter (lambda (f) (string-prefix? "y_" f)) json-files)
) ;for-each

(display "roundtrip: pass ")
(display rt-pass)
(display ", fail ")
(display rt-fail)
(newline)
(for-each (lambda (m) (display m) (newline)) (reverse rt-failures))
(newline)
(display "yes: pass ")
(display yes-pass)
(display ", fail ")
(display yes-fail)
(newline)
(display "no: pass ")
(display no-pass)
(display ", fail ")
(display no-fail)
(newline)
(display "impl (不计): ")
(display impl-count)
(newline)
(display "failed cases:")
(newline)
(for-each (lambda (msg) (display msg) (newline)) (reverse failures))
