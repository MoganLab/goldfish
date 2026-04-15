(import (liii check)
  (liii goldfix-record)
) ;import

(check-set-mode! 'report-failed)

;; make-fix-token
;; 创建带位置信息的 token。

(let ((token (make-fix-token :type
               'open-paren
               :offset
               3
               :end
               4
               :line
               2
               :column
               2
               :text
               "("
             ) ;make-fix-token
      ) ;token
     ) ;
  (check (fix-token? token) => #t)
  (check (fix-token-type token)
    =>
    'open-paren
  ) ;check
  (check (fix-token-offset token) => 3)
  (check (fix-token-end token) => 4)
  (check (fix-token-line token) => 2)
  (check (fix-token-column token) => 2)
  (check (fix-token-text token) => "(")
) ;let

;; make-fix-line
;; 创建按物理行分组后的 token 行。

(let* ((token (make-fix-token :type
                'other
                :offset
                0
                :end
                6
                :line
                1
                :column
                0
                :text
                "define"
              ) ;make-fix-token
       ) ;token
       (line (make-fix-line :number
               1
               :start-offset
               0
               :first-code-token
               token
               :tokens
               (list token)
             ) ;make-fix-line
       ) ;line
      ) ;
  (check (fix-line? line) => #t)
  (check (fix-line-number line) => 1)
  (check (fix-line-start-offset line)
    =>
    0
  ) ;check
  (check (fix-line-first-code-token line)
    =>
    token
  ) ;check
  (check (fix-line-tokens line)
    =>
    (list token)
  ) ;check
) ;let*

;; make-open-frame / make-pending-close
;; open-frame 表示一个仍然打开的左括号环境。
;; pending-close 表示一个需要等下一行缩进确认的右括号。

(let* ((frame (make-open-frame :offset
                0
                :line
                1
                :column
                0
                :tag-name
                "begin"
              ) ;make-open-frame
       ) ;frame
       (pending (make-pending-close :frame
                  frame
                  :offset
                  20
                  :end
                  21
                  :line
                  2
                  :column
                  15
                ) ;make-pending-close
       ) ;pending
      ) ;
  (check (open-frame? frame) => #t)
  (check (open-frame-offset frame) => 0)
  (check (open-frame-line frame) => 1)
  (check (open-frame-column frame) => 0)
  (check (open-frame-tag-name frame)
    =>
    "begin"
  ) ;check
  (check (pending-close? pending) => #t)
  (check (pending-close-frame pending)
    =>
    frame
  ) ;check
  (check (pending-close-offset pending)
    =>
    20
  ) ;check
  (check (pending-close-end pending)
    =>
    21
  ) ;check
  (check (pending-close-line pending)
    =>
    2
  ) ;check
  (check (pending-close-column pending)
    =>
    15
  ) ;check
) ;let*

;; make-fix-edit / make-repair-report
;; edit 记录源码变更，report 汇总修复结果。

(let* ((insert-edit (make-fix-edit :kind
                      'insert
                      :offset
                      10
                      :text
                      ")"
                      :reason
                      "eof"
                      :open-offset
                      0
                    ) ;make-fix-edit
       ) ;insert-edit
       (delete-edit (make-fix-edit :kind
                      'delete
                      :start
                      5
                      :end
                      6
                      :reason
                      "extra-close"
                      :open-offset
                      #f
                    ) ;make-fix-edit
       ) ;delete-edit
       (report (make-repair-report :ok?
                 #t
                 :edits
                 (list insert-edit delete-edit)
                 :diagnostics
                 '()
               ) ;make-repair-report
       ) ;report
      ) ;
  (check (fix-edit? insert-edit) => #t)
  (check (fix-edit-kind insert-edit)
    =>
    'insert
  ) ;check
  (check (fix-edit-offset insert-edit)
    =>
    10
  ) ;check
  (check (fix-edit-text insert-edit)
    =>
    ")"
  ) ;check
  (check (fix-edit-reason insert-edit)
    =>
    "eof"
  ) ;check
  (check (fix-edit-kind delete-edit)
    =>
    'delete
  ) ;check
  (check (fix-edit-start delete-edit)
    =>
    5
  ) ;check
  (check (fix-edit-end delete-edit) => 6)
  (check (repair-report? report) => #t)
  (check (repair-report-ok? report) => #t)
  (check (repair-report-edits report)
    =>
    (list insert-edit delete-edit)
  ) ;check
  (check (repair-report-diagnostics report)
    =>
    '()
  ) ;check
) ;let*

(check-report)
