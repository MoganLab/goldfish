;; (liii ini-file) test file — SRFI 233: INI files
;;
;; Tests for make-ini-file-generator and make-ini-file-accumulator.

;; ==== 函数分类索引 ====

;; 一、INI 读取（Generator）
;;   make-ini-file-generator  - 从输入端口创建 INI 行生成器

;; 二、INI 写入（Accumulator）
;;   make-ini-file-accumulator  - 向输出端口创建 INI 行累加器

;; ==== 单元测试 ====

(import (liii check) (liii ini-file) (liii base))

(check-set-mode! 'report-failed)

;; ==== Helper ====

(define (generator->list gen)
  (let loop
    ((result '()))
    (let ((item (gen)))
      (if (eof-object? item) (reverse result) (loop (cons item result)))
    ) ;let
  ) ;let
) ;define

;; ==== 1. make-ini-file-generator: 基础键值对（无 section） ====

(let ((gen (make-ini-file-generator (open-input-string "name=Alice\nage=30\n"))))
  (check (gen) => '(#f name "Alice"))
  (check (gen) => '(#f age "30"))
  (check (eof-object? (gen)) => #t)
) ;let

;; ==== 2. make-ini-file-generator: 带 section ====

(let ((gen (make-ini-file-generator (open-input-string "[database]\nhost=localhost\nport=5432\n")
           ) ;make-ini-file-generator
      ) ;gen
     ) ;
  (check (gen) => '(database host "localhost"))
  (check (gen) => '(database port "5432"))
  (check (eof-object? (gen)) => #t)
) ;let

;; ==== 3. make-ini-file-generator: section 前的键值对属于 #f section ====

(let ((gen (make-ini-file-generator (open-input-string "global_key=global_val\n[section1]\nkey1=val1\n")
           ) ;make-ini-file-generator
      ) ;gen
     ) ;
  (check (gen) => '(#f global_key "global_val"))
  (check (gen) => '(section1 key1 "val1"))
  (check (eof-object? (gen)) => #t)
) ;let

;; ==== 4. make-ini-file-generator: 注释和空行 ====

(let ((gen (make-ini-file-generator (open-input-string "; this is a comment\n\nkey=val\n"))
      ) ;gen
     ) ;
  (check (gen) => '(#f key "val"))
  (check (eof-object? (gen)) => #t)
) ;let

;; ==== 5. make-ini-file-generator: = 前后空白忽略 ====

(let ((gen (make-ini-file-generator (open-input-string "  key  =  value here  \n"))))
  (check (gen) => '(#f key "value here"))
  (check (eof-object? (gen)) => #t)
) ;let

;; ==== 6. make-ini-file-generator: 多个 = 只有第一个是分隔符 ====

(let ((gen (make-ini-file-generator (open-input-string "url=http://example.com?key=val\n"))
      ) ;gen
     ) ;
  (check (gen) => '(#f url "http://example.com?key=val"))
  (check (eof-object? (gen)) => #t)
) ;let

;; ==== 7. make-ini-file-generator: 无 = 的行，值为 #f ====

(let ((gen (make-ini-file-generator (open-input-string "standalone_key\n[section]\nanother\n")
           ) ;make-ini-file-generator
      ) ;gen
     ) ;
  (check (gen) => '(#f standalone_key #f))
  (check (gen) => '(section another #f))
  (check (eof-object? (gen)) => #t)
) ;let

;; ==== 8. make-ini-file-generator: 自定义分隔符 ====

(let ((gen (make-ini-file-generator (open-input-string "key: value\n") #\: #\#)))
  (check (gen) => '(#f key "value"))
  (check (eof-object? (gen)) => #t)
) ;let

;; ==== 9. make-ini-file-generator: 自定义注释符 ====

(let ((gen (make-ini-file-generator (open-input-string "# comment\nkey=val\n") #\= #\#)
      ) ;gen
     ) ;
  (check (gen) => '(#f key "val"))
  (check (eof-object? (gen)) => #t)
) ;let

;; ==== 10. make-ini-file-generator: 多个 section ====

(let ((gen (make-ini-file-generator (open-input-string "[section1]\nk1=v1\n[section2]\nk2=v2\n")
           ) ;make-ini-file-generator
      ) ;gen
     ) ;
  (check (gen) => '(section1 k1 "v1"))
  (check (gen) => '(section2 k2 "v2"))
  (check (eof-object? (gen)) => #t)
) ;let

;; ==== 11. make-ini-file-generator: SRFI 233 示例（集成测试） ====

(let* ((ini-content "; Be sure to update the following line\nlast_modified_date=2022-08-10\n[other]\nquiet=/qa\n[install]\nallusers=true\napplicationusers=allusers\nclientauditingport=6420\ndatabasedb=boe120\nenablelogfile=true\ninstall.lp.fr.selected=true\ninstallswitch=server\nnsport=6400\nwebsite_metabase_number=true\n[features]\nremove=wcadotnet,webapplicationcontainer\n"
       ) ;ini-content
       (gen (make-ini-file-generator (open-input-string ini-content)))
       (results (generator->list gen))
      ) ;
  (check (length results) => 12)
  (check (list-ref results 0) => '(#f last_modified_date "2022-08-10"))
  (check (list-ref results 1) => '(other quiet "/qa"))
  (check (list-ref results 2) => '(install allusers "true"))
  (check (list-ref results 3) => '(install applicationusers "allusers"))
  (check (list-ref results 4) => '(install clientauditingport "6420"))
  (check (list-ref results 11)
    =>
    '(features remove "wcadotnet,webapplicationcontainer")
  ) ;check
) ;let*

;; ==== 12. make-ini-file-generator: 行内注释 ====

(let ((gen (make-ini-file-generator (open-input-string "key=val ; inline comment\n")))
     ) ;
  (check (gen) => '(#f key "val"))
  (check (eof-object? (gen)) => #t)
) ;let

;; ==== 13. make-ini-file-accumulator: 基础键值对写入 ====

(let ((result (call-with-output-string (lambda (out)
                                         (let ((acc (make-ini-file-accumulator out)))
                                           (acc '(#f key "value"))
                                         ) ;let
                                       ) ;lambda
              ) ;call-with-output-string
      ) ;result
     ) ;
  (check result => "key=value\n")
) ;let

;; ==== 14. make-ini-file-accumulator: 带 section 写入 ====

(let ((result (call-with-output-string (lambda (out)
                                         (let ((acc (make-ini-file-accumulator out)))
                                           (acc '(section1 k1 "v1"))
                                           (acc '(section1 k2 "v2"))
                                         ) ;let
                                       ) ;lambda
              ) ;call-with-output-string
      ) ;result
     ) ;
  (check result => "[section1]\nk1=v1\nk2=v2\n")
) ;let

;; ==== 15. make-ini-file-accumulator: section 切换 ====

(let ((result (call-with-output-string (lambda (out)
                                         (let ((acc (make-ini-file-accumulator out)))
                                           (acc '(section1 k1 "v1"))
                                           (acc '(section2 k2 "v2"))
                                         ) ;let
                                       ) ;lambda
              ) ;call-with-output-string
      ) ;result
     ) ;
  (check result => "[section1]\nk1=v1\n[section2]\nk2=v2\n")
) ;let

;; ==== 16. make-ini-file-accumulator: 注释写入 ====

(let ((result (call-with-output-string (lambda (out)
                                         (let ((acc (make-ini-file-accumulator out)))
                                           (acc "this is a comment")
                                         ) ;let
                                       ) ;lambda
              ) ;call-with-output-string
      ) ;result
     ) ;
  (check result => "; this is a comment\n")
) ;let

;; ==== 17. make-ini-file-accumulator: EOF 处理 ====

(let ((acc-result #f))
  (let ((result (call-with-output-string (lambda (out)
                                           (let ((acc (make-ini-file-accumulator out)))
                                             (acc '(#f key "val"))
                                             (set! acc-result (acc (eof-object)))
                                           ) ;let
                                         ) ;lambda
                ) ;call-with-output-string
        ) ;result
       ) ;
    ;; closes binding list
    (check (eof-object? acc-result) => #t)
    (check result => "key=val\n")
  ) ;let
) ;let

;; ==== 18. make-ini-file-accumulator: 自定义分隔符 ====

(let ((result (call-with-output-string (lambda (out)
                                         (let ((acc (make-ini-file-accumulator out #\: #\#)))
                                           (acc '(#f key "value"))
                                         ) ;let
                                       ) ;lambda
              ) ;call-with-output-string
      ) ;result
     ) ;
  (check result => "key:value\n")
) ;let

;; ==== 19. make-ini-file-accumulator: 自定义注释符 ====

(let ((result (call-with-output-string (lambda (out)
                                         (let ((acc (make-ini-file-accumulator out #\= #\#)))
                                           (acc "a comment")
                                         ) ;let
                                       ) ;lambda
              ) ;call-with-output-string
      ) ;result
     ) ;
  (check result => "# a comment\n")
) ;let

;; ==== 20. make-ini-file-accumulator: #f section 不写 section 头 ====

(let ((result (call-with-output-string (lambda (out)
                                         (let ((acc (make-ini-file-accumulator out)))
                                           (acc '(#f key1 "val1"))
                                           (acc '(#f key2 "val2"))
                                         ) ;let
                                       ) ;lambda
              ) ;call-with-output-string
      ) ;result
     ) ;
  (check result => "key1=val1\nkey2=val2\n")
) ;let

;; ==== 21. 往返测试：generator 读出再用 accumulator 写回 ====

(let* ((ini-content "[database]\nhost=localhost\nport=5432\n[server]\nport=8080\n")
       (gen (make-ini-file-generator (open-input-string ini-content)))
       (items (generator->list gen))
       (result (call-with-output-string (lambda (out)
                                          (let ((acc (make-ini-file-accumulator out)))
                                            (for-each acc items)
                                          ) ;let
                                        ) ;lambda
               ) ;call-with-output-string
       ) ;result
      ) ;
  (check result => ini-content)
) ;let*

;; ==== 22. make-ini-file-generator: 注释在 section 行上 ====

(let ((gen (make-ini-file-generator (open-input-string "[section] ; comment\nkey=val\n"))
      ) ;gen
     ) ;
  (check (gen) => '(section key "val"))
  (check (eof-object? (gen)) => #t)
) ;let

;; ==== 23. 往返测试：standalone key（值为 #f） ====

(let* ((ini-content "standalone_key\n[section]\nanother\n")
       (gen (make-ini-file-generator (open-input-string ini-content)))
       (items (generator->list gen))
       (result (call-with-output-string (lambda (out)
                                          (let ((acc (make-ini-file-accumulator out)))
                                            (for-each acc items)
                                          ) ;let
                                        ) ;lambda
               ) ;call-with-output-string
       ) ;result
      ) ;
  (check result => ini-content)
) ;let*

(check-report)
