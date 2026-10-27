(import (scheme base)
        (liii check)
        (liii pi web-search config)
) ;import

(check-set-mode! 'report-failed)

;; pi-load-web-search-config
;; 从显式路径加载
(define (find-web-search-fixture)
  (let loop ((candidates '("tools/pi/tests/fixtures/web-search.json"
                           "tests/fixtures/web-search.json"
                           "fixtures/web-search.json"
                           "../fixtures/web-search.json"
                           "../../fixtures/web-search.json"
                           "../../../fixtures/web-search.json"
                           "../../../../tools/pi/tests/fixtures/web-search.json")))
    (cond ((null? candidates) (error 'file-not-found "web-search.json fixture not found"))
          ((file-exists? (car candidates)) (car candidates))
          (else (loop (cdr candidates))))))

(let ((cfg (pi-load-web-search-config
             (list (cons "PI_WEB_SEARCH_CONFIG" (find-web-search-fixture))
               (cons "HOME" "/nonexistent-gf-test")
               (cons "PI_CODING_AGENT_DIR" "/nonexistent-gf-test")))))
  (check-true (pair? cfg))
  (check (cdr (assoc "provider" cfg)) => "deepseek"))

;; 路径不存在时返回 #f
(check-false (pi-load-web-search-config
               '(("PI_WEB_SEARCH_CONFIG" . "/nonexistent-gf-test/ws.json")
                 ("HOME" . "/nonexistent-gf-test")
                 ("PI_CODING_AGENT_DIR" . "/nonexistent-gf-test"))))

(check-report)
