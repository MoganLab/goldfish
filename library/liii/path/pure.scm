;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii path pure)
  (export path-name path-stem path-suffix path-suffixes path-with-name
    path-with-stem path-with-suffix path-relative-to path-starts-with? path-join
    path-parent path-parents path-absolute path-expanduser path-resolve
  ) ;export
  (import (scheme base)
    (liii base)
    (liii error)
    (liii os)
    (liii string)
    (liii vector)
    (liii path parse)
    (liii path record)
    (liii path component)
  ) ;import
  (begin

    ;; ; 规范化绝对路径:消除 . 段、折叠 .. 段。
    ;; ; 不解析符号链接(无 realpath 原语,与 pathlib strict 语义有差异)。
    (define (normalize-absolute p)
      (let* ((pp (path-absolute p))
             (segs (vector->list (path-record-parts pp)))
             (root (path-record-root pp))
             (type (path-record-type pp))
             (drive (path-record-drive pp))
            ) ;
        (let loop
          ((rest segs) (acc '()))
          (cond
           ((null? rest) (make-path-record (list->vector (reverse acc)) type drive root))
           ((string=? (car rest) "..")
            (if (null? acc)
              ;; .. 在根之上:丢弃(不能越过根)
              (loop (cdr rest) acc)
              (loop (cdr rest) (cdr acc))
            ) ;if
           ) ;
           (else (loop (cdr rest) (cons (car rest) acc)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    ;; ; 对齐 pathlib.PurePath.relative_to(*other)
    ;; ; 差异:要求两者 anchor(drive/root/type)完全一致 — posix 上 root 必须相同,
    ;; ; Windows 上 drive 和 root 都必须相同。否则抛 value-error。
    (define (path-relative-to p base)
      (let* ((pp (path p))
             (bp (path base))
             (p-segs (path-record-parts pp))
             (b-segs (path-record-parts bp))
             (p-drive (path-record-drive pp))
             (b-drive (path-record-drive bp))
             (p-root (path-record-root pp))
             (b-root (path-record-root bp))
             (pn (vector-length p-segs))
             (bn (vector-length b-segs))
            ) ;
        (define (same-anchor?)
          (and (string=? p-drive b-drive)
            (eq? p-root b-root)
            (eq? (path-record-type pp) (path-record-type bp))
          ) ;and
        ) ;define
        (define (prefix-match?)
          ;; 检查 b-segs 是 p-segs 的前缀
          (let loop
            ((i 0))
            (cond ((= i bn) #t)
                  ((not (string=? (vector-ref p-segs i) (vector-ref b-segs i))) #f)
                  (else (loop (+ i 1)))
            ) ;cond
          ) ;let
        ) ;define
        (define (remaining-segments)
          ;; 取 p-segs 从 bn 起的剩余段
          (let* ((rlen (- pn bn)) (rest (make-vector rlen)))
            (let fill
              ((j 0))
              (if (= j rlen)
                rest
                (begin
                  (vector-set! rest j (vector-ref p-segs (+ bn j)))
                  (fill (+ j 1))
                ) ;begin
              ) ;if
            ) ;let
          ) ;let*
        ) ;define
        (cond ((not (same-anchor?))
               (value-error "path-relative-to: paths do not share an anchor")
              ) ;
              ((> bn pn) (value-error "path-relative-to: path is not relative to base"))
              ((not (prefix-match?))
               (value-error "path-relative-to: path is not relative to base")
              ) ;
              ((= bn pn) (path "."))
              (else (make-path-record (remaining-segments) (path-record-type pp) "" #f))
        ) ;cond
      ) ;let*
    ) ;define

    ;; ; 对齐 pathlib.PurePath.is_relative_to(other)
    ;; ; 返回 #t 当且仅当 p 是 base 本身或 base 的后代路径。
    (define (path-starts-with? p base)
      (let* ((pp (path p))
             (bp (path base))
             (p-segs (path-record-parts pp))
             (b-segs (path-record-parts bp))
             (p-drive (path-record-drive pp))
             (b-drive (path-record-drive bp))
             (p-root (path-record-root pp))
             (b-root (path-record-root bp))
             (pn (vector-length p-segs))
             (bn (vector-length b-segs))
            ) ;
        (define (same-anchor?)
          (and (string=? p-drive b-drive)
            (eq? p-root b-root)
            (eq? (path-record-type pp) (path-record-type bp))
          ) ;and
        ) ;define
        (define (prefix-match?)
          (let loop
            ((i 0))
            (cond ((= i bn) #t)
                  ((not (string=? (vector-ref p-segs i) (vector-ref b-segs i))) #f)
                  (else (loop (+ i 1)))
            ) ;cond
          ) ;let
        ) ;define
        (and (same-anchor?) (>= pn bn) (prefix-match?))
      ) ;let*
    ) ;define

    ;; ; 对齐 pathlib.PurePath.joinpath(*pathsegments)
    ;; ; 差异:Windows 下 drive 继承/重置规则复杂 ——
    ;; ;   完整绝对段(C:\b 或 \\srv\sh\a)替换 acc;
    ;; ;   drive-relative 段(D:rel):drive 与 acc 不同→替换,相同→追加(C:\base.joinpath('C:rel')=>C:\base\rel);
    ;; ;   current-drive root 段(\b):继承 acc drive,重置主体(C:\base.joinpath('\b')=>C:\b)。
    ;; ; posix 退化为"绝对段替换 / 相对段追加"。
    (define (path-join base . segments)
      (let ((base-rec (path base)))
        (define (append-parts acc seg)
          (define (drop-dots v)
            (vector-filter (lambda (x) (not (string=? x "."))) v)
          ) ;define
          (let ((acc-parts (drop-dots (path-record-parts acc)))
                (seg-parts (drop-dots (path-record-parts seg)))
               ) ;
            (make-path-record (vector-append acc-parts seg-parts)
              (path-record-type acc)
              (path-record-drive acc)
              (path-record-root acc)
            ) ;make-path-record
          ) ;let
        ) ;define
        (define (join-one acc seg)
          (let ((seg-type (path-record-type seg))
                (seg-drive (path-record-drive seg))
                (seg-root (path-record-root seg))
                (acc-drive (path-record-drive acc))
               ) ;
            (cond
              ;; 完整绝对段:替换 acc
              ((path-absolute? seg) seg)
              ;; Windows drive-relative 段(有 drive 无 root)
              ((and (eq? seg-type 'windows) (not (string-null? seg-drive)) (not seg-root))
               (if (string=? seg-drive acc-drive) (append-parts acc seg) seg)
              ) ;
              ;; Windows current-drive root 段(无 drive 有 root):继承 acc drive,重置主体
              ((and (eq? seg-type 'windows) (string-null? seg-drive) seg-root)
               (make-path-record (path-record-parts seg) 'windows acc-drive seg-root)
              ) ;
              ;; 纯相对段:追加 parts
              (else (append-parts acc seg))
            ) ;cond
          ) ;let
        ) ;define
        (let loop
          ((acc base-rec) (rest segments))
          (if (null? rest)
            acc
            (if (and (string? (car rest)) (string-null? (car rest)))
              (loop acc (cdr rest))
              (loop (join-one acc (path (car rest))) (cdr rest))
            ) ;if
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; ; 对齐 pathlib.PurePath.parent
    ;; ; 差异:posix 根 "/" 的 parent 是自身;Windows UNC anchor (\\srv\sh) parent 是自身,
    ;; ; drive-absolute 单段(C:\Users) parent 回到 "C:\",drive-relative(C:foo) parent 是 "C:",
    ;; ; 相对单段(a) parent 是当前目录 "."。
    (define (path-parent p)
      (let* ((pp (path p))
             (parts (path-record-parts pp))
             (type (path-record-type pp))
             (drive (path-record-drive pp))
             (root (path-record-root pp))
             (n (vector-length parts))
            ) ;
        (cond
          ;; 无段:根路径(/、C:\、\\srv\sh)或空相对路径,parent 为自身
          ((= n 0) pp)
          ;; 仅一段:取决于是否有 anchor。
          ;;  - 有 root(绝对 /a、C:\Users、\foo、\\srv\sh\a):parent 为其 anchor
          ;;  - 无 root 但有 drive(drive-relative C:foo):parent 为 "C:"(pathlib 语义)
          ;;  - 无 root 无 drive(相对 a、foo):parent 为当前目录 "."
          ((= n 1)
           (if root
             (cond ((and (eq? type 'posix) (string-null? drive))
                    (make-path-record #() 'posix "" #\/)
                   ) ;
                   ;; UNC 单段(\\srv\sh\a → \\srv\sh):drive 存的是 share anchor
                   ;; ("\\server\share" 以 \\ 开头,与单字母盘符区分),直接回到
                   ;; 空 parts + 原 drive/root。
                   ((unc-prefix? drive) (make-path-record #() 'windows drive root))
                   ((not (string-null? drive)) (path-of-drive (string-ref drive 0)))
                   ;; 无 drive 但有 root:windows current-drive root (\foo → \)
                   (else (make-path-record #() 'windows "" root))
             ) ;cond
             (if (not (string-null? drive))
               ;; drive-relative 单段:C:foo → C:(保留 drive、root 空、parts 空)
               (make-path-record #() 'windows drive #f)
               (path ".")
             ) ;if
           ) ;if
          ) ;
          ;; 多段:去掉末段,保留 drive/root
          (else (make-path-record (vector-drop-right parts 1) type drive root))
        ) ;cond
      ) ;let*
    ) ;define

    ;; ; 对齐 pathlib.PurePath.parents
    ;; ; 差异:绝对路径终止于根/anchor;相对路径终止于当前目录 "."。
    ;; ; 跨平台行为一致 — 基于 path-parent 反复回溯。
    (define (path-parents p)
      (let ((start (path p)))
        (let loop
          ((cur start) (acc '()))
          (let* ((par (path-parent cur))
                 (par-str (path->string par))
                 (cur-str (path->string cur))
                ) ;
            (cond
              ;; parent 等于自身(cur 已是根/anchor,或 cur 自身是 "."):停止,不纳入。
              ;; 覆盖 'a'(parent=.)→ 但 cur≠.,故不命中此条;覆盖 '.'(parent=.,cur=.)→ 命中,返回 ()。
              ((string=? par-str cur-str) (list->vector (reverse acc)))
              ;; 相对路径回溯到当前目录且 cur 自身不是 ".":纳入 "." 后停止。
              ((string=? par-str ".") (list->vector (reverse (cons (path ".") acc))))
              (else (loop par (cons par acc)))
            ) ;cond
          ) ;let*
        ) ;let
      ) ;let
    ) ;define

    ;; ; 对齐 pathlib.Path.absolute()
    ;; ; 差异:相对路径拼 cwd,绝对路径原样返回。不解析符号链接(区别于 path-resolve)。
    (define (path-absolute p)
      (let ((pp (path p)))
        (if (path-absolute? pp) pp (path (path-join (path-cwd) pp)))
      ) ;let
    ) ;define

    ;; ; 对齐 pathlib.Path.expanduser()(仅 ~ 部分;不支持 ~user)
    ;; ; 差异:~ 展开为 path-home() 的结果 — POSIX 上是 HOME,Windows 上是 USERPROFILE。
    (define (path-expanduser p)
      (let* ((pp (path p)) (s (path->string pp)))
        (if (string-starts? s "~")
          (let* ((rest (substring s 1 (string-length s))) (home-str (path->string (path-home))))
            (path (cond ((string-null? rest) home-str)
                        ((string-starts? rest "/") (string-append home-str rest))
                        (else (string-append home-str "/" rest))
                  ) ;cond
            ) ;path
          ) ;let*
          pp
        ) ;if
      ) ;let*
    ) ;define

    ;; ; 对齐 pathlib.Path.resolve(strict=False) 简化版
    ;; ; 差异:绝对化 + 折叠 . / .. 段;不解析符号链接(无 realpath 原语)。
    ;; ; Windows 与 posix 均适用,基于 path-absolute 取 cwd 作种子。
    (define (path-resolve p)
      (normalize-absolute p)
    ) ;define

  ) ;begin
) ;define-library
