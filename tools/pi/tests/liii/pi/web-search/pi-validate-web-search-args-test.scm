(import (liii check) (liii list) (liii pi web-search))

(check-set-mode! 'report-failed)

;; pi-validate-web-search-args
;; 校验 web_search 工具参数：query 必填非空字符串，urls 可选字符串列表且最多 20 项。

;; 合法参数
(check (pi-validate-web-search-args '(("query" . "goldfish scheme"))) => #t)
(check (pi-validate-web-search-args '(("query" . "goldfish scheme")
                                        ("urls" . ("https://example.com"))
                                      ) ;list
       ) ;pi-validate-web-search-args
  => #t
) ;check
(check (pi-validate-web-search-args '(("query" . "goldfish scheme") ("urls" . ())))
  => #t
) ;check

;; 恰好 20 个 URL 合法
(check (pi-validate-web-search-args
         (list (cons "query" "goldfish scheme")
           (cons "urls" (map (lambda (i) (string-append "https://example.com/" (number->string i)))
                          (iota 20)
                        ) ;map
           ) ;cons
         ) ;list
       ) ;pi-validate-web-search-args
  => #t
) ;check

;; 缺少 query / query 非字符串
(check-catch 'type-error (pi-validate-web-search-args '()))
(check-catch 'type-error (pi-validate-web-search-args '(("urls" . ("https://example.com")))))
(check-catch 'type-error (pi-validate-web-search-args '(("query" . 42))))

;; query 为空字符串或纯空白
(check-catch 'value-error (pi-validate-web-search-args '(("query" . ""))))
(check-catch 'value-error (pi-validate-web-search-args '(("query" . "   "))))

;; urls 形状非法
(check-catch 'type-error (pi-validate-web-search-args '(("query" . "q") ("urls" . "https://example.com"))))
(check-catch 'type-error (pi-validate-web-search-args '(("query" . "q") ("urls" . (42)))))
(check-catch 'type-error (pi-validate-web-search-args '(("query" . "q") ("urls" . ("ok" 42)))))

;; urls 超过 20 项
(check-catch 'value-error (pi-validate-web-search-args
                            (list (cons "query" "q")
                              (cons "urls" (map (lambda (i) (number->string i)) (iota 21)))
                            ) ;list
                          ) ;pi-validate-web-search-args
) ;check-catch

(check-report)
