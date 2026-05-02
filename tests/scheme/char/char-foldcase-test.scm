(import (liii check) (scheme char))
(check-set-mode! 'report-failed)
;; char-foldcase
;; 执行字符的大小写折叠
;;
;; 语法
;; ----
;; (char-foldcase char) → char
;;
;; 参数
;; ----
;; char : character
;; 要转换的字符
;;
;; 返回值
;; ------
;; character
;; 转换后的字符
;;
;; 注意
;; ----
;; - 遵循 Unicode Simple Case Folding 规范
;; - 对于大部分字符，行为与 char-downcase 相同
;; - 对于 Cherokee 大写字母、部分希腊字母变体等，行为与 char-downcase 不同
;;
;; 错误处理
;; ------
;; type-error
;; 参数必须是字符类型，否则会抛出异常

;; 基本 ASCII 测试
(check (char-foldcase #\A) => #\a)
(check (char-foldcase #\Z) => #\z)
(check (char-foldcase #\a) => #\a)
(check (char-foldcase #\z) => #\z)
;; 非字母字符测试
(check (char-foldcase #\5) => #\5)
(check (char-foldcase #\space) => #\space)
(check (char-foldcase #\!) => #\!)

;; 差异字符测试：foldcase 与 downcase 不同
;; U+00B5 (MICRO SIGN) → U+03BC (GREEK SMALL LETTER MU)
(check (char-foldcase #\µ) => #\μ)
;; U+0130 (LATIN CAPITAL LETTER I WITH DOT ABOVE) 保持不变
(check (char-foldcase #\İ) => #\İ)
;; U+017F (LATIN SMALL LETTER LONG S) → U+0073 (s)
(check (char-foldcase #\ſ) => #\s)
;; U+0345 (COMBINING GREEK YPOGEGRAMMENI) → U+03B9 (GREEK SMALL LETTER IOTA)
(check (char-foldcase #\ͅ) => #\ι)
;; U+03C2 (GREEK SMALL LETTER FINAL SIGMA) → U+03C3 (GREEK SMALL LETTER SIGMA)
(check (char-foldcase #\ς) => #\σ)
;; 希腊字母变体
(check (char-foldcase #\ϐ) => #\β)
(check (char-foldcase #\ϑ) => #\θ)
(check (char-foldcase #\ϕ) => #\φ)
(check (char-foldcase #\ϖ) => #\π)
(check (char-foldcase #\ϰ) => #\κ)
(check (char-foldcase #\ϱ) => #\ρ)
(check (char-foldcase #\ϵ) => #\ε)

;; Cherokee 大写字母保持不变
(check (char-foldcase #\Ꭰ) => #\Ꭰ)
(check (char-foldcase #\Ᏽ) => #\Ᏽ)
;; Cherokee 小写扩展映射到大写
(check (char-foldcase #\ᏸ) => #\Ᏸ)
(check (char-foldcase #\ᏽ) => #\Ᏽ)
;; Cherokee 小写字母映射到大写
(check (char-foldcase #\ꭰ) => #\Ꭰ)
(check (char-foldcase #\ꮿ) => #\Ꮿ)

;; Cyrillic 历史字母
(check (char-foldcase #\ᲀ) => #\в)
(check (char-foldcase #\ᲁ) => #\д)
(check (char-foldcase #\ᲂ) => #\о)
(check (char-foldcase #\ᲃ) => #\с)
(check (char-foldcase #\ᲄ) => #\т)
(check (char-foldcase #\ᲅ) => #\т)
(check (char-foldcase #\ᲆ) => #\ъ)
(check (char-foldcase #\ᲇ) => #\ѣ)
(check (char-foldcase #\ᲈ) => #\ꙋ)

;; 其他差异字符
(check (char-foldcase #\ẛ) => #\ṡ)
(check (char-foldcase #\ι) => #\ι)
(check (char-foldcase #\ΐ) => #\ΐ)
(check (char-foldcase #\ΰ) => #\ΰ)
(check (char-foldcase #\ﬅ) => #\ﬆ)

;; 错误处理测试
(check-catch 'type-error (char-foldcase "A"))
(check-catch 'type-error (char-foldcase 65))
(check-catch 'type-error (char-foldcase 'A))
(check-report)
