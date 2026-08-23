(define (gfo-base-dir)
  (let ((xdg (getenv "XDG_CACHE_HOME")))
    (string-append
      (if (and xdg (not (string=? xdg ""))) xdg
        (string-append (or (getenv "HOME") "/tmp") "/.cache"))
      "/goldfish/ccache")))

;;; --- cache identity: pipeline version directory -------------------------
;;; The cache directory is qualified by a content hash of every pipeline
;;; input that can change how sources are expanded/compiled: the bootstrap
;;; chain, the kernel artifact, the expander lib and the compiler.  Touching
;;; any of them moves all cached artifacts into a fresh directory; git
;;; operations do NOT change the tag (content-addressed, not time-based),
;;; so a checkout/rebase keeps its cache.  Keep the file list in sync with
;;; the bootstrap chain (LAYER.md L1..L5).

(define *gfo-version-cache* #f)

(define (gfo-sort-strings ls)
  ;; insertion sort -- gfo.scm runs before any base-layer sort exists.
  (if (null? ls)
    '()
    (let ((m (car ls)))
      (for-each (lambda (x) (when (string<? x m) (set! m x))) ls)
      (cons m (gfo-sort-strings (let remove ((ls ls) (acc '()))
                                  (cond ((null? ls) (reverse acc))
                                        ((string=? (car ls) m) (append (reverse acc) (cdr ls)))
                                        (else (remove (cdr ls) (cons (car ls) acc))))))))))

(define (gfo-scm-files rel)
  ;; .scm names directly under REL, sorted (directory order is unstable).
  (let* ((base (car (g_load-path)))
         (v (catch #t (lambda () (g_listdir (string-append base "/" rel))) (lambda args #f))))
    (if (vector? v)
      (gfo-sort-strings
        (let loop ((i (- (vector-length v) 1)) (acc '()))
          (if (< i 0)
            acc
            (let ((n (vector-ref v i)))
              (loop (- i 1)
                    (if (and (> (string-length n) 4)
                             (string=? (substring n (- (string-length n) 4)) ".scm"))
                      (cons n acc)
                      acc))))))
      '())))

(define (gfo-locate rel)
  ;; Minimal lookup against the load path: returns the first existing path,
  ;; or REL itself (a missing file then contributes "-" to the fingerprint).
  (let loop ((dirs (g_load-path)))
    (cond ((null? dirs) rel)
          ((file-exists? (string-append (car dirs) "/" rel))
           (string-append (car dirs) "/" rel))
          (else (loop (cdr dirs))))))

(define (gfo-pipeline-fingerprint)
  ;; Aggregate sha256 over every pipeline input; the name is mixed in per
  ;; file and a missing file contributes "-".  The s7 version joins the
  ;; mix so an interpreter change moves the directory too.
  (define files
    (append
      (list "liii/boot.scm" "core/gfo.scm"
            "liii/prelude.scm" "liii/reader.scm"
            "liii/host-abi.scm" "liii/bootstrap-macros.scm"
            "expander/kernel-combined.scm" "compiler.scm")
      (map (lambda (n) (string-append "expander/lib/" n)) (gfo-scm-files "expander/lib"))
      (map (lambda (n) (string-append "compiler/" n)) (gfo-scm-files "compiler"))))
  (define (feed acc f)
    (let ((h (catch #t
               (lambda () (g_sha256-by-file (gfo-locate f)))
               (lambda args #f))))
      (string-append acc f ":" (or h "-") ";")))
  (let loop ((fs files) (acc (string-append "*s7*:" (*s7* 'version) ";")))
    (if (null? fs)
      (g_sha256 acc)
      (loop (cdr fs) (feed acc (car fs))))))

(define *gfo-version-cache* #f)

(define (gfo-version-tag)
  ;; 12-hex prefix of the pipeline fingerprint; memoized for the process.
  (or *gfo-version-cache*
      (let* ((fp (catch #t
                   (lambda () (substring (gfo-pipeline-fingerprint) 0 12))
                   (lambda args "bootstrap0")))
             (tag (string-append "v" fp)))
        (set! *gfo-version-cache* tag)
        tag)))

(define (gfo-dir)
  (string-append (gfo-base-dir) "/" (gfo-version-tag)))

(define (gfo-separator? c)
  (or (char=? c #\/) (char=? c #\\)))

(define (gfo-key path)
  (let ((n (string-length path)))
    (let loop ((i 0) (start 0) (parts '()))
      (if (> i n)
        (if (null? parts)
          "root"
          (let ((rev (reverse parts)))
            (let lp ((acc (car rev)) (rest (cdr rev)))
              (if (null? rest)
                acc
                (lp (string-append acc "/" (car rest)) (cdr rest))))))
        (if (or (= i n) (gfo-separator? (string-ref path i)))
          (let ((comp (substring path start i)))
            (loop (+ i 1) (+ i 1)
                  (if (or (string=? comp "")
                          (string=? comp ".")
                          (and (> (string-length comp) 0)
                               (char=? (string-ref comp (- (string-length comp) 1)) #\:)))
                    parts
                    (cons (if (string=? comp "..") "_dotdot" comp) parts))))
          (loop (+ i 1) start parts))))))

(define (gfo-path path)
  (string-append (gfo-dir) "/" (gfo-key path) ".gfo"))

(define (gfo-ensure-parent! dir file)
  (if (not (file-exists? dir)) (g_mkdir dir))
  (let ((rel (substring file (string-length dir))))
    (let ((n (string-length rel)))
      (let loop ((i 1))
        (let ((j (let lp ((k i))
                   (if (or (= k n) (char=? (string-ref rel k) #\/))
                     k
                     (lp (+ k 1))))))
          (when (< j n)
            (let ((d (string-append dir (substring rel 0 j))))
              (if (not (file-exists? d)) (g_mkdir d))
              (loop (+ j 1)))))))))

(define (gfo-stamp path)
  (list (g_path-getmtime path) (g_path-getsize path)))

;;; gfo-format-version : cache record layout version.  A record carrying a
;;; different version is a cache miss and regenerates (users never clear
;;; caches by hand).  0 marks the in-development format; 1 is reserved for
;;; the first release format, so dev caches invalidate on release.
(define gfo-format-version 0)

(define (gfo-valid? gfo-file stamp)
  (and (file-exists? gfo-file)
       (let ((rec (call-with-input-file gfo-file (lambda (p) (car (read-forms p))))))
         (and (pair? rec) (eq? (car rec) 'gfo)
              (equal? (cadr rec) gfo-format-version)
              (equal? (caddr rec) stamp)))))

(define (gfo-read gfo-file)
  (let ((rec (car (read-forms (open-input-file gfo-file)))))
    (cadddr rec)))

(define (gfo-write! gfo-file stamp payload)
  (if (getenv "GOLDFISH_CACHE_READONLY") #f
      (begin
        (gfo-ensure-parent! (gfo-dir) gfo-file)
        (let ((old-length (*s7* 'print-length)))
          (let-set! *s7* 'print-length 1000000)
          (let ((tmp (string-append gfo-file ".tmp")))
            (call-with-output-file tmp
              (lambda (p)
                (if (defined? 'write-roundtrip) (write-roundtrip (list 'gfo gfo-format-version stamp payload) p)
                    (write (list 'gfo gfo-format-version stamp payload) p))))
            (g_rename tmp gfo-file))
          (let-set! *s7* 'print-length old-length)))))
