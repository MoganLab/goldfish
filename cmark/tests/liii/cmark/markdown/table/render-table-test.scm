;; render-table
;; 渲染表格节点为行列表：各列按最大终端显示宽度补齐空格。
;; 表头、分隔线和表体的管道符纵向对齐，中文按双宽计算。
;;
;; 语法
;; ----
;; (render-table node)
;;
;; 参数
;; ----
;; node : pair
;; (table (align ...) (thead (tr ...)) (tbody (tr ...) ...)) 节点。
;;
;; 返回值
;; ----
;; list
;; 行字符串列表。
;;
;; 错误处理
;; ----
;; 表格结构非法时抛出 md-stem-error。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown table))

(check-set-mode! 'report-failed)

(check-true (procedure? render-table))

;; ---- 基本表格与四种对齐 ----
(check (render-table '(table (align default) (thead (tr (th "a"))) (tbody)))
  => '("| a   |" "| --- |")
) ;check
(check
  (render-table
    '(table (align left center right default)
       (thead (tr (th "a") (th "b") (th "c") (th "d")))
       (tbody (tr (td "1") (td "2") (td "3") (td "4")))
     )
  ) ;render-table
  => '("| a   | b   | c   | d   |" "| :-- | :-: | --: | --- |" "| 1   | 2   | 3   | 4   |")
) ;check

;; ---- 空表体 ----
(check
  (render-table '(table (align default default)
                    (thead (tr (th "a") (th "b")))
                    (tbody)))
  => '("| a   | b   |" "| --- | --- |")
) ;check

;; ---- 空单元格 ----
(check
  (render-table '(table (align default default)
                    (thead (tr (th "a") (th)))
                    (tbody (tr (td "1") (td "2")))
                  )
  ) ;render-table
  => '("| a   |     |" "| --- | --- |" "| 1   | 2   |")
) ;check

;; ---- 管道符转义 ----
(check
  (render-table '(table (align default)
                    (thead (tr (th "a|b")))
                    (tbody (tr (td "1")))
                  )
  ) ;render-table
  => '("| a\\|b |" "| ---- |" "| 1    |")
) ;check

;; ---- 单元格内行内内容 ----
(check
  (render-table '(table (align default)
                    (thead (tr (th (strong "a"))))
                    (tbody (tr (td (link "u" "" "链接"))))
                  )
  ) ;render-table
  => '("| **a**     |" "| --------- |" "| [链接](u) |")
) ;check
;; 单元格内 code 含管道符：转义为 \| 保持单元格结构不拆分
;; （正向解析把 \| 留在 code 字面量中，code 含裸 | 的表格单元格
;;   本就无法语义无损往返，正向转换也不可能产生该结构）
(check
  (render-table '(table (align default)
                    (thead (tr (th "x")))
                    (tbody (tr (td (code "c|d"))))
                  )
  ) ;render-table
  => '("| x      |" "| ------ |" "| `c\\|d` |")
) ;check

;; ---- 长短内容与 Unicode 显示宽度 ----
(check
  (render-table
    '(table (align default right)
       (thead (tr (th "Name") (th "值")))
       (tbody (tr (td "A") (td "中文内容")) (tr (td "Long name") (td "x")))
     )
  ) ;render-table
  => '("| Name      | 值       |"
       "| --------- | -------: |"
       "| A         | 中文内容 |"
       "| Long name | x        |")
) ;check
(check
  (render-table
    '(table (align left center right default)
       (thead (tr (th "long") (th "middle") (th "r") (th "x")))
       (tbody (tr (td "a") (td "b") (td "right") (td "default")))
     )
  ) ;render-table
  => '("| long | middle | r     | x       |"
       "| :--- | :----: | ----: | ------- |"
       "| a    | b      | right | default |")
) ;check

;; 畸形手工 stem 的列数不一致时，以已有单元格为准安全补齐，不产生负宽度。
(check
  (render-table
    '(table (align default)
       (thead (tr (th "a") (th "b")))
       (tbody (tr (td "longer"))))
  ) ;render-table
  => '("| a      | b   |" "| ------ | --- |" "| longer |     |")
) ;check

;; ---- 非法表格结构 ----
(check-catch 'md-stem-error (render-table '(table)))
(check-catch 'md-stem-error
  (render-table '(table (align bogus) (thead (tr (th "a"))) (tbody)))
) ;check-catch
(check-catch 'md-stem-error
  (render-table '(table (align default) (thead) (tbody)))
) ;check-catch

(check-report)
