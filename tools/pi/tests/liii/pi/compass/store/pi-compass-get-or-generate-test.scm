(import (liii check)
        (liii string)
        (liii path)
        (liii pi compass project)
        (liii pi compass generate)
        (liii pi compass store)
        (liii pi compass types)
) ;import

(check-set-mode! 'report-failed)

(define dir (path-join (path-temp-dir) "gf-compass-store-test"))
(define dir-str (path->string dir))
(define codemap-file (pi-compass-default-codemap-path dir-str))
(define src-a (path->string (path-join dir "src" "a.js")))
(define src-b (path->string (path-join dir "src" "b.js")))
(define readme (path->string (path-join dir "README.md")))

(define (cleanup!)
  (for-each
    (lambda (f) (when (path-file? (path f)) (path-unlink (path f) #t)))
    (list codemap-file src-a src-b readme)
  ) ;for-each
  (for-each
    (lambda (d) (when (path-dir? (path d)) (path-rmdir (path d))))
    (list (path->string (path-join dir "src"))
      (path->string (path-join dir ".pi"))
      dir-str
    ) ;list
  ) ;for-each
) ;define

(define (setup!)
  (cleanup!)
  (path-mkdir dir :parents #t :exist-ok #t)
  (path-mkdir (path (path->string (path-join dir "src"))) :parents #t :exist-ok #t)
  (path-write-text (path src-a) "// a\n")
  (path-write-text (path readme) "# demo\n")
) ;define

(dynamic-wind (lambda () #f)
  (lambda ()
    (setup!)
    (let* ((project (pi-compass-detect-project dir-str))
           (result (pi-compass-get-or-generate dir-str project codemap-file #f))
           (codemap (pi-compass-jref result "codemap" #f))
           (stamp (pi-compass-codemap-generated-at codemap))
           (hash (pi-compass-codemap-content-hash codemap))
          ) ;
      ;; 首次生成并落盘。
      (check (pi-compass-jref result "fromCache" #f) => #f)
      (check (path-file? (path codemap-file)) => #t)
      (check (string-ends? (path-as-posix (path codemap-file)) ".pi/codemap.json") => #t)

      ;; 顶层三段结构。
      (let ((entry (pi-compass-load-codemap codemap-file)))
        (check (pi-compass-object? entry) => #t)
        (check (pi-compass-codemap? (pi-compass-jref entry "data" #f)) => #t)
        (check (pi-compass-jref entry "contentHash" #f) => hash)
        (check (pi-compass-jref entry "createdAt" #f) => stamp)
      ) ;let

      ;; 二次运行命中缓存：时间戳不变。
      (let* ((again (pi-compass-get-or-generate dir-str project codemap-file #f))
             (again-codemap (pi-compass-jref again "codemap" #f))
            ) ;
        (check (pi-compass-jref again "fromCache" #f) => #t)
        (check (pi-compass-codemap-generated-at again-codemap) => stamp)
      ) ;let*

      ;; 深度 2 改动 → 哈希变化 → 重新生成。
      (path-write-text (path src-b) "// b\n")
      (let* ((changed (pi-compass-get-or-generate dir-str project codemap-file #f))
             (changed-codemap (pi-compass-jref changed "codemap" #f))
            ) ;
        (check (pi-compass-jref changed "fromCache" #f) => #f)
        (check (not (string=? (pi-compass-codemap-content-hash changed-codemap) hash))
          =>
          #t
        ) ;check
      ) ;let*

      ;; --refresh 无条件重新生成（即便无改动）。
      (let* ((before (pi-compass-codemap-content-hash
                       (pi-compass-jref (pi-compass-get-or-generate
                                          dir-str
                                          project
                                          codemap-file
                                          #f
                                        ) ;pi-compass-get-or-generate
                         "codemap"
                         #f
                       ) ;pi-compass-jref
                     ) ;pi-compass-codemap-content-hash
             ) ;before
             (refreshed (pi-compass-get-or-generate dir-str project codemap-file #t))
            ) ;
        (check (pi-compass-jref refreshed "fromCache" #f) => #f)
        (check (pi-compass-codemap-content-hash
                 (pi-compass-jref refreshed "codemap" #f)
               ) ;pi-compass-codemap-content-hash
          =>
          before
        ) ;check
      ) ;let*
    ) ;let*
  ) ;lambda
  (lambda () (cleanup!))
) ;dynamic-wind

(check-report)
