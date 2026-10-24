(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-from-string
;; 从字符串构造路径值，与 path 函数等价。
;;
;; 语法
;; ----
;; (path-from-string string-path)
;;
;; 参数
;; ----
;; string-path : string?
;; 字符串形式的路径。
;;
;; 返回值
;; ----
;; path-value
;; 返回对应的路径值。
;;
;; 示例
;; ----
;; (path->string (path-from-string "archive.tar.gz")) => "archive.tar.gz"

(check (path->string (path-from-string "archive.tar.gz")) => "archive.tar.gz")

(when (os-windows?)
  ;; s7 的 port-filename / argv 在 Windows 上返回正斜杠绝对路径,
  ;; parse-windows-path 必须同时识别 / 和 \,否则 string-split-vec 一刀不切
  (check (path->string (path-from-string "C:/Users/foo/bar.scm"))
    =>
    "C:\\Users\\foo\\bar.scm"
  ) ;check
) ;when

;; Round-trip 不变量:对一组 pathlib 风格输入,序列化后再解析、再序列化,
;; 第二次结果应当与第一次相同(等价于 record 字段稳定)。
;; 失败说明 parse-windows-path 与 path->string 不对称(典型症状是 drive/root 丢失)。

(define (roundtrip-stable? s)
  (let* ((p1 (path s)) (s1 (path->string p1)) (p2 (path s1)) (s2 (path->string p2)))
    (string=? s1 s2)
  ) ;let*
) ;define

(when (not (os-windows?))
  (check-true (roundtrip-stable? "/a/b/c"))
  (check-true (roundtrip-stable? "/tmp/demo.txt"))
  (check-true (roundtrip-stable? "a/b/c"))
  (check-true (roundtrip-stable? "."))
  (check-true (roundtrip-stable? "/"))
  (check-true (roundtrip-stable? "a//b"))
  (check-true (roundtrip-stable? "/tmp/"))
) ;when

(when (os-windows?)
  (check-true (roundtrip-stable? "C:\\Users\\foo"))
  (check-true (roundtrip-stable? "C:/Users/foo"))
  (check-true (roundtrip-stable? "C:\\"))
  (check-true (roundtrip-stable? "C:foo"))
  (check-true (roundtrip-stable? "\\foo"))
  (check-true (roundtrip-stable? "\\\\srv\\sh\\a\\b"))
  (check-true (roundtrip-stable? "\\\\srv\\sh"))
  (check-true (roundtrip-stable? "a\\b\\c"))
) ;when

(check-report)
