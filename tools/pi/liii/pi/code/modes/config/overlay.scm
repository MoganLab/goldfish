;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `gf pi config` 的只读覆盖层：`v` 键查看某个条目对应的配置文件。
;; 纯渲染，无副作用；文件缺失（虚拟条目）时说明数值来源。
(define-library (liii pi code modes config overlay)
  (import (scheme base)
    (liii base)
    (liii list)
    (liii string)
    (liii unicode)
    (liii tui)
    (liii pi code config user-fs)
  ) ;import
  (export pi-config-render-overlay pi-config-overlay-step)
  (begin

    (define %accent "\x1b;[35m")
    (define %reset "\x1b;[0m")
    (define %dim "\x1b;[2m")

    ;; ---------- 文本工具 ----------

    (define (%fit text width)
      (let* ((plain (strip-ansi text)) (len (display-width plain)))
        (cond ((= width 0) "")
              ((<= len width) (string-append text (utf8-make-string (- width len) #\space)))
              ((<= (string-length plain) width) plain)
              (else (truncate-to-width plain width))
        ) ;cond
      ) ;let*
    ) ;define

    ;; 安全窗口：从 offset 起取至多 rows 行（take 在长度不足时会报错）
    (define (%slice lines offset rows)
      (let ((rest (if (>= offset (length lines)) '() (list-tail lines offset))))
        (if (> (length rest) rows) (take rest rows) rest)
      ) ;let
    ) ;define

    (define (%pad lines rows)
      (let ((n (length lines)))
        (cond ((= n rows) lines)
              ((> n rows) (take lines rows))
              (else
                (append lines (map (lambda (i) "") (iota (- rows n))))
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; ---------- 尺寸 ----------

    (define (%overlay-width width)
      (max 20 (min (- width 6) 96))
    ) ;define

    (define (%overlay-height content-lines height)
      (max 4 (min (+ content-lines 3) (- height 4)))
    ) ;define

    ;; ---------- 内容 ----------

    (define (%overlay-title view)
      (string-append (pi-user-dict-ref view "path" "")
        " · "
        (if (pi-user-dict-ref view "exists" #f) "read-only" "no file")
      ) ;string-append
    ) ;define

    ;; 虚拟条目（无文件）：说明有效值来自内置目录/注册表
    (define (%missing-lines view)
      (let* ((kind (pi-user-dict-ref view "kind" "provider"))
             (id (pi-user-dict-ref view "id" ""))
             (path (pi-user-dict-ref view "path" ""))
            ) ;
        (list (string-append path " not found")
          ""
          (if (string=? kind "provider")
            (string-append "provider " id " · values from builtin registry")
            (string-append "model " id " · values from builtin catalog")
          ) ;if
          ""
          (string-append %dim "space toggles · q closes" %reset)
        ) ;list
      ) ;let*
    ) ;define

    (define (%content-lines view)
      (if (pi-user-dict-ref view "exists" #f)
        (string-split (or (pi-user-dict-ref view "content" #f) "") "\n")
        (%missing-lines view)
      ) ;if
    ) ;define

    ;; ---------- 主渲染 ----------

    ;; 居中覆盖层：恰好 height 行，每行不超过 width
    ;; offset 为垂直滚动行数，message 显示在底部提示行
    (define* (pi-config-render-overlay view width height (offset 0) (message ""))
      (let* ((content (%content-lines view))
             (ow (%overlay-width width))
             ;; 框体 = 顶边 + 内容行 + 底边 + 提示行
             (oh (%overlay-height (length content) height))
             (inner (- ow 4))
             (rows (max 1 (- oh 3)))
             (visible (%pad (%slice content offset rows) rows))
             (pad-x (max 0 (quotient (- width ow) 2)))
             (pad-y (max 0 (quotient (- height oh) 2)))
             (hint (if (string=? message "")
                     " ↑↓/jk scroll · g/G top/bottom · q close"
                     (string-append " " message)
                   ) ;if
             ) ;hint
             (head
               (string-append %accent
                 "┌"
                 (%fit (string-append " " (%overlay-title view)) (+ inner 2))
                 "┐"
                 %reset
               ) ;string-append
             ) ;head
             (foot (string-append %accent "└" (utf8-make-string (+ inner 2) #\─) "┘" %reset)
             ) ;foot
             (body
               (map (lambda (line)
                      (string-append %accent
                        "│"
                        %reset
                        " "
                        (%fit line inner)
                        " "
                        %accent
                        "│"
                        %reset
                      ) ;string-append
                    ) ;lambda
                 visible
               ) ;map
             ) ;body
             (box (append (list head) body (list foot (%fit hint ow))))
             (blank (%fit "" width))
             (raw-lines
               (append (map (lambda (i) blank) (iota pad-y))
                 (map (lambda (line) (string-append (%fit "" pad-x) line (%fit "" pad-x))) box)
                 (map (lambda (i) blank) (iota (max 0 (- height pad-y oh))))
               ) ;append
             ) ;raw-lines
             (len (length raw-lines))
            ) ;
        (cond ((<= height 0) '())
              ((> len height) (take raw-lines height))
              ((< len height) (append raw-lines (make-list (- height len) blank)))
              (else raw-lines)
        ) ;cond
      ) ;let*
    ) ;define*

    ;; ---------- 键盘交互 ----------

    ;; 返回 (list 新view 新offset)；按 q/escape 返回 (list #f 0)
    (define (pi-config-overlay-step view offset height key)
      (let* ((c (cond ((char? key) (string key))
                      ((symbol? key) (symbol->string key))
                      (else key)
                ) ;cond
             ) ;c
             (content (%content-lines view))
             (oh (%overlay-height (length content) height))
             (rows (max 1 (- oh 3)))
            ) ;
        (cond ((or (eq? key 'escape) (equal? c "q")) (list #f 0))
              ((or (eq? key 'down) (equal? c "j")) (list view (+ offset 1)))
              ((or (eq? key 'up) (equal? c "k")) (list view (max 0 (- offset 1))))
              ((equal? c "g") (list view 0))
              ((equal? c "G") (list view (max 0 (- (length content) rows))))
              (else (list view offset))
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
