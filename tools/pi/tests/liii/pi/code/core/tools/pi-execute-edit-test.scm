(import (liii check)
  (liii string)
  (liii path)
  (liii pi code core tools edit)
  (liii pi code core tools)
  (scheme file)
) ;import

(check-set-mode! 'report-failed)

;; pi-execute-edit
;; 执行 edit 工具，在文件中精确替换一个或多个代码块。
;;
;; 语法
;; ----
;; (pi-execute-edit args . options) -> string
;;
;; 参数
;; ----
;; args : alist
;; 包含 path 和 edits；每项 edit 包含 oldText 与 newText。
;; options : list
;; 可选关键字参数，如 :cwd 和 :timed-out?。
;;
;; 返回值
;; ----
;; string
;; 替换成功后的提示文本。oldText 缺失、命中多次、编辑区域重叠或内容未变化时抛出错误。
;;
;; 示例
;; ----
;; (pi-execute-edit `(("path" . "a.scm")
;;                    ("edits" . ((("oldText" . "old") ("newText" . "new"))))))
(let* ((target (path->string (path-join (path-temp-dir) "test-pi-edit.txt")))
       (_ (call-with-output-file target (lambda (p) (display "hello old text" p))))
       (res
         (pi-execute-edit
           `(("path" . ,target)
             ("edits" (("oldText" . "old") ("newText" . "new"))))
         ) ;pi-execute-edit
       ) ;res
      ) ;
  (check-true (string? res))
  (delete-file target)
) ;let*

(let ((target (path->string (path-join (path-temp-dir) "test-pi-edit-err.txt"))))
  ;; 命中多次相同的 oldText 报错
  (pi-execute-write (list (cons "path" target) (cons "content" "dup\ndup\n")))
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" target)
        (cons "edits" (list (list (cons "oldText" "dup") (cons "newText" "new"))))
      ) ;list
    ) ;pi-execute-edit
  ) ;check-catch

  ;; 重叠修改报错
  (pi-execute-write (list (cons "path" target) (cons "content" "ABCDEFGH")))
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" target)
        (cons "edits"
          (list (list (cons "oldText" "ABCD") (cons "newText" "1234"))
            (list (cons "oldText" "CDEF") (cons "newText" "5678"))
          ) ;list
        ) ;cons
      ) ;list
    ) ;pi-execute-edit
  ) ;check-catch

  ;; oldText 与 newText 相同报错
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" target)
        (cons "edits"
          (list (list (cons "oldText" "ABCDEFGH") (cons "newText" "ABCDEFGH")))
        ) ;cons
      ) ;list
    ) ;pi-execute-edit
  ) ;check-catch

  ;; edits 列表为空报错
  (check-catch 'error
    (pi-execute-edit (list (cons "path" target) (cons "edits" '())))
  ) ;check-catch

  ;; oldText 为空字符串报错
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" target)
        (cons "edits" (list (list (cons "oldText" "") (cons "newText" "foo"))))
      ) ;list
    ) ;pi-execute-edit
  ) ;check-catch

  ;; 模拟超时
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" target)
        (cons "edits" (list (list (cons "oldText" "ABC") (cons "newText" "XYZ"))))
      ) ;list
      :timed-out? #t
    ) ;pi-execute-edit
  ) ;check-catch
  (delete-file target)
) ;let

;; 单块、多块替换与不匹配错误
(let ((test-edit-file "tests/liii/pi-edit-fixture.txt"))
  (pi-execute-write (list (cons "path" test-edit-file)
                      (cons "content" "line 1\nline 2: AAA\nline 3\nline 4: BBB\nline 5")
                    ) ;list
  ) ;pi-execute-write
  (let ((msg
          (pi-execute-edit
            (list (cons "path" test-edit-file)
              (cons "edits"
                (list (list (cons "oldText" "line 2: AAA") (cons "newText" "line 2: MODIFIED_A"))
                ) ;list
              ) ;cons
            ) ;list
          ) ;pi-execute-edit
        ) ;msg
       ) ;
    (check msg
      =>
      (string-append "Successfully replaced 1 block(s) in " test-edit-file ".")
    ) ;check
    (let ((c (pi-execute-read (list (cons "path" test-edit-file)))))
      (check-true (string-contains? c "line 2: MODIFIED_A"))
    ) ;let
  ) ;let
  (let ((msg
          (pi-execute-edit
            (list (cons "path" test-edit-file)
              (cons "edits"
                (list (list (cons "oldText" "line 1") (cons "newText" "line 1: FIRST"))
                  (list (cons "oldText" "line 4: BBB") (cons "newText" "line 4: SECOND"))
                ) ;list
              ) ;cons
            ) ;list
          ) ;pi-execute-edit
        ) ;msg
       ) ;
    (check msg
      =>
      (string-append "Successfully replaced 2 block(s) in " test-edit-file ".")
    ) ;check
    (let ((c (pi-execute-read (list (cons "path" test-edit-file)))))
      (check-true (string-contains? c "line 1: FIRST"))
      (check-true (string-contains? c "line 4: SECOND"))
    ) ;let
  ) ;let
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" test-edit-file)
        (cons "edits"
          (list (list (cons "oldText" "NON_EXISTENT") (cons "newText" "NEW")))
        ) ;cons
      ) ;list
    ) ;pi-execute-edit
  ) ;check-catch
  (delete-file test-edit-file)
) ;let

(check-report)
