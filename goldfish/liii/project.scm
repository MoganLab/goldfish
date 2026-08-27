(define-library (liii project)
  (import (scheme base) (liii base) (liii os) (liii path) (liii sort) (liii string) (liii list))
  (export project-root gfproject-load-config gfproject-tools gfproject-tool-imports function-libraries function-doc-hint)
  (begin

    (define (normalize-string s)
      (if (string? s) (string-append s "") s))

    (define (join-path a b)
      (let ([as (normalize-string (if (string? a) a (path->string a)))]
            [bs (normalize-string (if (string? b) b (path->string b)))])
        (cond [(string=? as "") bs]
              [(char=? (string-ref as (- (string-length as) 1)) #\/) (string-append as bs)]
              [else (string-append as "/" bs)])))

    (define (project-root)
      (let loop ([dir (g_getcwd)])
        (cond [(or (not dir) (not (string? dir)) (string=? dir "")) #f]
              [(g_isfile (normalize-string (join-path dir "gfproject.scm"))) dir]
              [else
               (let* ([p (path dir)]
                      [parent (path->string (path-parent p))])
                 (if (or (string=? parent dir) (string=? parent "") (string=? parent "/"))
                   #f
                   (loop parent)))])))

    ;; -- gfproject --

    (define (file-exists? p) (and (string? p) (g_isfile p)))

    (define (read-gfproject path)
      (if (not (file-exists? path)) '()
        (catch #t
          (lambda ()
            (let ((port (open-input-file path)))
              (let ((form (read port)))
                (close-input-port port)
                (if (and (pair? form) (eq? (car form) 'gfproject)) (cdr form) '()))))
          (lambda _ '()))))

    (define (assoc-ref alist key) (let ((e (assoc key alist))) (if e (cdr e) #f)))

    (define (find-lib-gfproject)
      (let ((lib (g_goldfish-library)))
        (if (not (string? lib)) #f
          (let ((p1 (join-path lib "gfproject.scm"))
                (p2 (join-path (path->string (path-parent (path lib))) "gfproject.scm")))
            (cond [(file-exists? p1) p1] [(file-exists? p2) p2] [else #f])))))

    (define (find-local-gfproject)
      (let ((cwd (g_getcwd)))
        (if (not (string? cwd)) #f
          (let ((p (join-path cwd "gfproject.scm"))) (if (file-exists? p) p #f)))))

    (define (tools-alist form)
      (let ((e (assoc 'tools form))) (if e (cdr e) '())))

    (define (alist-merge base overlay)
      (let loop ([bs base] [acc overlay])
        (if (null? bs) acc
          (let* ([kv (car bs)] [k (car kv)] [v (cdr kv)] [ex (assoc k acc)])
            (if ex
              (let ((merged-v (if (and (pair? v) (pair? (cdr ex)) (every pair? v) (every pair? (cdr ex)))
                                (alist-merge v (cdr ex)) (cdr ex))))
                (loop (cdr bs) (cons (cons k merged-v) (filter (lambda (e) (not (eq? (car e) k))) acc))))
              (loop (cdr bs) (cons kv acc)))))))

    (define (json-escape s)
      (let loop ([i 0] [acc '()])
        (if (>= i (string-length s)) (list->string (reverse acc))
          (let ((c (string-ref s i)))
            (cond [(char=? c #\") (loop (+ i 1) (cons #\" (cons #\\ acc)))]
                  [(char=? c #\\) (loop (+ i 1) (cons #\\ (cons #\\ acc)))]
                  [else (loop (+ i 1) (cons c acc))])))))

    (define (value->json v)
      (cond [(string? v) (string-append "\"" (json-escape v) "\"")]
            [(symbol? v) (string-append "\"" (json-escape (symbol->string v)) "\"")]
            [(number? v) (number->string v)]
            [(boolean? v) (if v "true" "false")]
            [(null? v) "null"]
            [(pair? v)
             (cond [(every pair? v)
                    (string-append "{" (string-join (map (lambda (kv) (string-append "\"" (json-escape (symbol->string (car kv))) "\":" (value->json (cdr kv)))) v) ",") "}")]
                   [else (string-append "[" (string-join (map value->json v) ",") "]")])]
            [else "null"]))

    (define (merge-tools lib-tools local-tools)
      (let loop ([ls local-tools] [acc lib-tools])
        (if (null? ls) acc
          (let* ([kv (car ls)] [k (car kv)] [v (cdr kv)] [ex (assoc k acc)])
            (if ex
              (loop (cdr ls) (cons (cons k (alist-merge (cdr ex) v)) (filter (lambda (e) (not (eq? (car e) k))) acc)))
              (loop (cdr ls) (cons kv acc)))))))

    (define (tools->json tools)
      (if (null? tools) "{}"
        (string-append "{" (string-join (map (lambda (tool) (string-append "\"" (json-escape (symbol->string (car tool))) "\":" (value->json (cdr tool)))) tools) ",") "}")))

    (define (gfproject-load-config)
      (let* ((lib-path (find-lib-gfproject))
             (local-path (find-local-gfproject))
             (lib-tools (if lib-path (tools-alist (read-gfproject lib-path)) '()))
             (local-tools (if local-path (tools-alist (read-gfproject local-path)) '())))
        (string-append "{\"tools\":" (tools->json (merge-tools lib-tools local-tools)) "}")))

    (define (gfproject-tools)
      (let* ((lib-path (find-lib-gfproject))
             (local-path (find-local-gfproject))
             (lib-tools (if lib-path (tools-alist (read-gfproject lib-path)) '()))
             (local-tools (if local-path (tools-alist (read-gfproject local-path)) '())))
        (merge-tools lib-tools local-tools)))

    ;; Tool dispatch interface for the host: given a command name, return
    ;; the import expressions of the tool defined by gfproject.scm files,
    ;; best candidate first (a local override is tried before the library
    ;; definition).  '() when CMD is not a project tool.  This replaces the
    ;; historical JSON views: the host only ever needed the organization /
    ;; module pair to build the import expression.
    (define (gfproject-tool-imports cmd)
      (define cmd-sym (string->symbol cmd))
      (define (entry where)
        (let ((path (where)))
          (and path
               (let ((e (assq cmd-sym (tools-alist (read-gfproject path)))))
                 (and e
                      (let* ((fields (cdr e))
                             (org (assq 'organization fields))
                             (mod (assq 'module fields)))
                        (and org mod
                             (pair? (cdr org)) (pair? (cdr mod))
                             (string-append "(import (" (symbol->string (cadr org))
                                            " " (symbol->string (cadr mod)) "))")))))))) 
      (let ((local (entry find-local-gfproject))
            (lib (entry find-lib-gfproject)))
        (cond ((and local lib) (list local lib))
              (local (list local))
              (lib (list lib))
              (else '()))))

    ;; -- function-libraries --

    (define (string-suffix? s suf)
      (and (>= (string-length s) (string-length suf))
           (string=? (substring s (- (string-length s) (string-length suf)) (string-length s)) suf)))

    (define (listdir-list dir)
      (if (not (path-dir? (path dir))) '()
        (let ((v (listdir dir))) (if (vector? v) (vector->list v) '()))))

    (define (sorted-children root)
      (list-sort string<? (filter (lambda (n) (path-dir? (path (join-path root n)))) (listdir-list root))))

    (define (sorted-scm-files dir)
      (list-sort string<? (filter (lambda (n) (and (string-suffix? n ".scm") (path-file? (path (join-path dir n))))) (listdir-list dir))))

    (define (export-matches? spec name)
      (cond [(symbol? spec) (string=? (symbol->string spec) name)]
            [(and (pair? spec) (eq? (car spec) 'rename) (= (length spec) 3))
             (let ((new (caddr spec)))
               (cond [(symbol? new) (string=? (symbol->string new) name)]
                     [(string? new) (string=? new name)]
                     [else #f]))]
            [else #f]))

    (define (library-exports? form name)
      (and (pair? form) (eq? (car form) 'define-library)
           (pair? (cdr form)) (pair? (cadr form)) (= (length (cadr form)) 2)
           (let loop-decls ([decls (cddr form)])
             (if (null? decls) #f
               (let ((d (car decls)))
                 (if (and (pair? d) (eq? (car d) 'export))
                   (let loop-specs ([specs (cdr d)])
                     (if (null? specs) (loop-decls (cdr decls))
                       (if (export-matches? (car specs) name) #t (loop-specs (cdr specs)))))
                   (loop-decls (cdr decls))))))))

    (define (file-exports? path name)
      (catch #t
        (lambda ()
          (let ((port (open-input-file path)))
            (let loop ()
              (let ((form (read port)))
                (cond [(eof-object? form) (close-input-port port) #f]
                      [(library-exports? form name) (close-input-port port) #t]
                      [else (loop)])))))
        (lambda _ #f)))

    (define (string->value s)
      (let ((n (string->number s)))
        (if (and n (integer? n)) n (string->symbol s))))

    (define (function-libraries name)
      (if (not (string? name)) '()
        (let ((load-path (g_load-path)) (res '()))
          (for-each
           (lambda (root)
             (when (and (string? root) (path-dir? (path root)))
               (for-each
                (lambda (group)
                  (let ((gdir (join-path root group)))
                    (for-each
                     (lambda (file)
                       (let ((full (join-path gdir file)))
                         (when (file-exports? full name)
                           (let* ((lib (substring file 0 (- (string-length file) 4)))
                                  (gs (string->value group))
                                  (ls (string->value lib))
                                  (key (string-append group "/" lib)))
                             (when (not (member key (map (lambda (e) (string-append (symbol->string (car e)) "/" (symbol->string (cadr e)))) res)))
                               (set! res (cons (list gs ls) res)))))))
                     (sorted-scm-files gdir))))
                (sorted-children root))))
           (if (list? load-path) load-path '()))
                     (list-sort (lambda (a b) (string<? (string-append (symbol->string (car a)) "/" (symbol->string (cadr a)))
                                             (string-append (symbol->string (car b)) "/" (symbol->string (cadr b)))))
                     res))))

    ;; -- function-doc-hint --

    ;; shell-quote for backtick command lines shown in error hints
    (define (shell-double-quote value)
      (let loop ((i 0) (acc (list #\")))
        (if (>= i (string-length value))
          (string-append (list->string (reverse acc)) "\"")
          (let ((ch (string-ref value i)))
            (loop (+ i 1)
                  (case ch
                    [(#\\) (cons #\\ (cons #\\ acc))]
                    [(#\") (cons #\" (cons #\\ acc))]
                    [(#\$) (cons #\$ (cons #\\ acc))]
                    [(#\`) (cons #\` (cons #\\ acc))]
                    [else (cons ch acc)]))))))

    (define (library-display-name entry)
      (string-append "(" (symbol->string (car entry)) " " (symbol->string (cadr entry)) ")"))

    ;; Full unbound-function hint text, or "" when NAME is not exported by
    ;; any visible library.  PROGRAM is the CLI name used in the suggested
    ;; doc commands.
    (define (function-doc-hint name program)
      (let ((libs (if (string? name) (function-libraries name) '())))
        (cond
          ((null? libs) "")
          ((null? (cdr libs))
           (let* ((entry (car libs))
                  (org (symbol->string (car entry)))
                  (mod (symbol->string (cadr entry))))
             (string-append "Hint: function `" name "` exists in library `(" org " " mod ")`.\n"
                            "Please import that library first: `(import (" org " " mod "))`.\n")))
          (else
           (string-append
            "Hint: function `" name "` exists in multiple visible libraries:\n"
            (apply string-append
                   (map (lambda (e) (string-append "  " (library-display-name e) "\n")) libs))
            "Try one of these commands to decide which library to use:\n"
            (apply string-append
                   (map (lambda (e)
                          (string-append "  " program " doc "
                                         (symbol->string (car e)) "/" (symbol->string (cadr e))
                                         " " (shell-double-quote name) "\n"))
                        libs)))))))
))
