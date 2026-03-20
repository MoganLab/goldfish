(import (liii check)
        (liii lint)
        (liii path)
) ;import

(check-set-mode! 'report-failed)

(display "Running complete lint tests with full result validation...\n")

; Test exact full results with complete structure validation
(let ((result1 (lint-check-brackets (path-read-text "tests/resources/200_14_valid.scm"))))
  (display "200_14_valid.scm: ")
  (write result1)
  (newline)
  (check result1 => '(matched))
) ;let

(let ((result2 (lint-check-brackets (path-read-text "tests/resources/200_14_unmatched_open.scm"))))
  (display "200_14_unmatched_open.scm: ")
  (write result2)
  (newline)
  (check result2 => '(unmatched (unclosed (13 1) ("list" 13 1))))
) ;let

(let ((result3 (lint-check-brackets (path-read-text "tests/resources/200_14_unmatched_close.scm"))))
  (display "200_14_unmatched_close.scm: ")
  (write result3)
  (newline)
  (check result3 => '(unmatched (unmatched-close (5 32) ("-" 5 23))))
) ;let

(let ((result4 (lint-check-brackets (path-read-text "tests/resources/200_14_mismatched.scm"))))
  (display "200_14_mismatched.scm: ")
  (write result4)
  (newline)
  (check result4 => '(unmatched (unclosed (9 6) anonymous)))
) ;let

(let ((result5 (lint-check-brackets (path-read-text "tests/resources/200_14_bad.scm"))))
  (display "200_14_bad.scm: ")
  (write result5)
  (newline)
  (check (car result5) => 'unmatched)
) ;let

(let ((result6 (lint-check-brackets (path-read-text "tests/resources/200_14_hash_unmatched.scm"))))
  (display "200_14_hash_unmatched.scm: ")
  (write result6)
  (newline)
  (check (car result6) => 'unmatched)
) ;let

(let ((result7 (lint-check-brackets (path-read-text "tests/resources/200_14_begin_define.scm"))))
  (display "200_14_begin_define.scm: ")
  (write result7)
  (newline)
  (check (car result7) => 'unmatched)
) ;let

(let ((result8 (lint-check-brackets (path-read-text "tests/resources/200_14_test1.scm"))))
  (display "200_14_test1.scm: ")
  (write result8)
  (newline)
  (check (car result8) => 'unmatched)
) ;let

; Valid files
(let ((result9 (lint-check-brackets (path-read-text "tests/resources/200_14_with_strings.scm"))))
  (display "200_14_with_strings.scm: ")
  (write result9)
  (newline)
  (check result9 => '(matched))
) ;let

(let ((result10 (lint-check-brackets (path-read-text "tests/resources/200_14_nested_constants.scm"))))
  (display "200_14_nested_constants.scm: ")
  (write result10)
  (newline)
  (check result10 => '(matched))
) ;let

(let ((result11 (lint-check-brackets (path-read-text "tests/resources/200_14_hash_valid.scm"))))
  (display "200_14_hash_valid.scm: ")
  (write result11)
  (newline)
  (check result11 => '(matched))
) ;let

(let ((result12 (lint-check-brackets (path-read-text "tests/resources/200_14_char_literal_correct.scm"))))
  (display "200_14_char_literal_correct.scm: ")
  (write result12)
  (newline)
  (check result12 => '(matched))
) ;let

(let ((result13 (lint-check-brackets (path-read-text "tests/resources/200_14_char_literal_quote.scm"))))
  (display "200_14_char_literal_quote.scm: ")
  (write result13)
  (newline)
  (check result13 => '(matched))
) ;let

(let ((result14 (lint-check-brackets (path-read-text "tests/resources/200_14_char_literal_quote_ch.scm"))))
  (display "200_14_char_literal_quote_ch.scm: ")
  (write result14)
  (newline)
  (check result14 => '(matched))
) ;let

(let ((result15 (lint-check-brackets (path-read-text "tests/resources/200_14_char_literal_schema.scm"))))
  (display "200_14_char_literal_schema.scm: ")
  (write result15)
  (newline)
  (check result15 => '(matched))
) ;let

(let ((result16 (lint-check-brackets (path-read-text "tests/resources/200_14_string_issue_complete_base64.scm"))))
  (display "200_14_string_issue_complete_base64.scm: ")
  (write result16)
  (newline)
  (check result16 => '(matched))
) ;let

(let ((result17 (lint-check-brackets (path-read-text "tests/resources/200_14_string_issue_plus.scm"))))
  (display "200_14_string_issue_plus.scm: ")
  (write result17)
  (newline)
  (check result17 => '(matched))
) ;let

(let ((result18 (lint-check-brackets (path-read-text "tests/resources/200_14_string_issue_slash.scm"))))
  (display "200_14_string_issue_slash.scm: ")
  (write result18)
  (newline)
  (check result18 => '(matched))
) ;let

(let ((result19 (lint-check-brackets (path-read-text "tests/resources/200_14_trie_comment_test.scm"))))
  (display "200_14_trie_comment_test.scm: ")
  (write result19)
  (newline)
  (check result19 => '(matched))
) ;let

(let ((result20 (lint-check-brackets (path-read-text "tests/resources/200_14_issue_analysis.scm"))))
  (display "200_14_issue_analysis.scm: ")
  (write result20)
  (newline)
  (check (car result20) => 'unmatched)
) ;let

(let ((result21 (lint-check-brackets (path-read-text "tests/resources/200_14_quote_test.scm"))))
  (display "200_14_quote_test.scm: ")
  (write result21)
  (newline)
  (check result21 => '(matched))
) ;let

(check-report)