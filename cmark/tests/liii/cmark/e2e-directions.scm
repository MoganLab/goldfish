;; e2e-directions
;; 成对资源的方向清单：集中登记少量 forward-only / reverse-only 例外
;; 及原因；未列出的资源默认 both（正向相等 + 反向语义往返）。
;; 本文件不是独立测试入口，由 e2e-commonmark-test.scm /
;; e2e-extensions-test.scm 加载。
;;
;; 条目格式：(资源相对路径 方向 "原因")
;; - both：Markdown -> md-stem 等于资源，且 md-stem -> Markdown -> md-stem 等于资源
;; - forward-only：只检查 Markdown -> md-stem（md-stem 未保留往返所需语义）
;; - reverse-only：只检查 md-stem -> Markdown -> md-stem
;; 例外必须引用具体资源路径，不得按章节批量排除；
;; 后续修复若消除原因，应删除对应条目恢复 both。

(define e2e-direction-entries
  '(
    ;; ---- 实体与字符信息丢失 ----
    ;; 正向解析把 HTML 实体（&ouml;）解码为字符（ö），md-stem 不保留实体形式
    ("2_Preliminaries/2.2_Backslash_escapes/Example14" forward-only
     "正向解析把 HTML 实体解码为字符，md-stem 不保留实体形式")

    ;; ---- 列表内 thematic-break 结构信息丢失 ----
    ;; md-stem 把列表项内的分隔线存为 (item (thematic-break))，
    ;; 反向渲染后重新解析为顶层 thematic-break，列表结构改变
    ("4_Leaf_blocks/4.1_Thematic_breaks/Example61" forward-only
     "列表项内的 thematic-break 反向渲染后脱离列表，结构无法保留")

    ;; ---- 代码块/代码段空格填充信息丢失 ----
    ;; 代码段字面量首尾空格会被解析器折叠填充，md-stem 保留原始空格数，
    ;; 反向渲染补齐的空格重新解析后空格数不一致
    ("4_Leaf_blocks/4.5_Fenced_code_blocks/Example138" forward-only
     "fence info 含空格被正向解析为代码段，反向填充空格数无法一致往返")
    ("6_Inlines/6.1_Code_spans/Example334" forward-only
     "代码段首尾空格填充后重新解析空格数不一致")

    ;; ---- 链接引用定义信息丢失 ----
    ;; 正向解析把链接引用定义消费为链接，md-stem 中保留为纯文本 [foo]；
    ;; 反向渲染转义方括号后被项目扩展重新识别为数学块，且重新解析
    ;; 会重建引用定义导致语义改变
    ("4_Leaf_blocks/4.7_Link_reference_definitions/Example197" forward-only
     "链接引用定义：md-stem 保留纯文本，反向转义方括号与数学块定界冲突")
    ("4_Leaf_blocks/4.7_Link_reference_definitions/Example199" forward-only
     "链接引用定义：md-stem 保留纯文本，反向转义方括号与数学块定界冲突")
    ("4_Leaf_blocks/4.7_Link_reference_definitions/Example201" forward-only
     "链接引用定义：md-stem 保留纯文本，反向转义方括号与数学块定界冲突")
    ("4_Leaf_blocks/4.7_Link_reference_definitions/Example211" forward-only
     "链接引用定义：md-stem 保留纯文本，反向转义方括号与数学块定界冲突")
    ("4_Leaf_blocks/4.7_Link_reference_definitions/Example212" forward-only
     "链接引用定义：md-stem 保留纯文本，反向转义方括号与数学块定界冲突")
    ("4_Leaf_blocks/4.7_Link_reference_definitions/Example213" forward-only
     "链接引用定义：md-stem 保留纯文本，反向转义方括号与数学块定界冲突")

    ;; ---- 列表标记种类信息丢失 ----
    ;; 正向把 - 与 + 两种无序列表标记分别记录为两个 bullet-list，
    ;; md-stem 不保留标记字符；反向统一为 - 后相邻列表被空行合并为单个 loose 列表
    ("5_Container_blocks/5.3_Lists/Example301" forward-only
     "相邻无序列表标记字符差异未保留，反向合并为单个 loose 列表")

    ;; ---- 强调定界歧义 ----
    ;; 文本首部裸 * 与紧随的 em/strong 在反向渲染时必须转义行首 *，
    ;; 转义后与强调开标记合并成长游程，无法恢复原语义
    ("6_Inlines/6.2_Emphasis_and_strong_emphasis/Example417" forward-only
     "strong 三层嵌套加尾随星号文本：定界符无法稳定往返")

    ;; ---- 链接/图片纯文本与数学块定界冲突 ----
    ;; md-stem 中含方括号纯文本或链接文本，反向转义方括号后被
    ;; 项目扩展重新识别为数学块，语义改变
    ("6_Inlines/6.3_Links/Example543" forward-only
     "方括号纯文本转义后与数学块定界冲突")
    ("6_Inlines/6.3_Links/Example545" forward-only
     "方括号纯文本转义后与数学块定界冲突")
    ("6_Inlines/6.3_Links/Example546" forward-only
     "含括号的链接标签纯文本转义后与数学块定界冲突")
    ("6_Inlines/6.3_Links/Example547" forward-only
     "含括号的链接标签纯文本转义后与数学块定界冲突")
    ("6_Inlines/6.3_Links/Example548" forward-only
     "多层方括号纯文本转义后与数学块定界冲突")
    ("6_Inlines/6.3_Links/Example551" forward-only
     "空方括号纯文本转义后与数学块定界冲突")
    ("6_Inlines/6.3_Links/Example552" forward-only
     "跨行空方括号纯文本转义后与数学块定界冲突")
    ("6_Inlines/6.3_Links/Example556" forward-only
     "链接后空方括号纯文本转义后与数学块定界冲突")
    ("6_Inlines/6.3_Links/Example559" forward-only
     "链接外层方括号转义后与数学块定界冲突")
    ("6_Inlines/6.3_Links/Example563" forward-only
     "方括号纯文本转义后与数学块定界冲突")
    ("6_Inlines/6.4_Images/Example587" forward-only
     "图片后空方括号纯文本转义后与数学块定界冲突")
    ;; 无效图片 ! + link 与有效图片 ![..](..) 同形，md-stem 区分二者但
    ;; 反向渲染同形，重新解析都成图片
    ("6_Inlines/6.4_Images/Example593" forward-only
     "感叹号加链接与图片同形，md-stem 区分但反向无法还原")
    ;; 无效邮件自动链接 <foo+@..> 解析为纯文本，反向转义尖括号后中间
    ;; 地址仍被裸链接识别，无法还原纯文本
    ("6_Inlines/6.5_Autolinks/Example606" forward-only
     "无效邮件自动链接纯文本转义尖括号后中间地址被裸链接重新识别")

    ;; ---- 扩展资源反向冲突 ----
    ;; code-block 语言 math 反向渲染为 ```math 被 math 扩展拦截为 math-block
    ("7_Math/7.3_Fenced_math/Math36" forward-only
     "code-block 语言 math 反向渲染为 ```math 被 math 扩展拦截为 math-block")
    ;; 无效 alert [!UNKNOWN] 在 md-stem 中是普通引用文本，反向转义方括号
    ;; 后被 math 扩展识别为数学块
    ("8_Extensions/8.5_Alerts/Alert10" forward-only
     "无效 alert 文本 [!UNKNOWN] 转义方括号后与数学块定界冲突")
    ("8_Extensions/8.5_Alerts/Alert11" forward-only
     "引用中的 [!NOTE] 纯文本转义方括号后与数学块定界冲突")
    ;; 综合样例中含方括号纯文本转义后与数学块定界冲突
    ("integration_08" forward-only
     "综合样例含方括号纯文本转义后与数学块定界冲突")
    ;; 紧凑外层列表内含 loose 子列表，子列表空行经反向渲染后重新解析
    ;; 使外层列表变 loose（与 Example301 同类的 tight/loose 穿透）
    ("integration_07" forward-only
     "紧凑外层列表含 loose 子列表，反向渲染后外层被重新解析为 loose")
  ) ;'
) ;define
