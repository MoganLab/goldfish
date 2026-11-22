# 综合文档十二：标题层级、任务流与表格

开场段落包含 [直接链接](https://example.com/start "Start")、![直接图](cover.png "Cover")、<span>行内 HTML</span>、行内公式 $a_n = a_{n-1} + 1$、实体 &amp;、邮箱 <team@example.com>，以及转义 \_不是强调\_。
这一段第二行继续补充，保持 soft break。
这一段第三行通过反斜杠换行\
第四行继续说明任务背景。

## 二级标题：项目目标

这里先引入参考式链接[规范][spec]和参考式图片![示意图][diagram]，随后放一个 `inline code` 片段，以及 <kbd>Ctrl+Shift+P</kbd> 这样的行内原始 HTML。

### 三级标题：无序任务列表

- [ ] 未完成项里有 **强调**、*斜体* 和 $x+y$。
- [x] 已完成项继续这一行
  这里是 lazy continuation，包含 [二次链接](https://example.com/next) 与 ![小图](mini.png)。
- 普通项中嵌套有序列表：
  1. 第一子项
  2. 第二子项中有 <span>HTML</span> 和 `gf test`
- 普通项中嵌套块引用：

  > 引用第一段，含有参考式[文档][doc]。
  >
  > 引用第二段，包含 $b^2 - 4ac$。

#### 四级标题：有序任务列表

1. [ ] 第一项要检查表格。
2. [x] 第二项说明 fenced math：

   ```math
   \sum_{k=1}^{n} k
   ```

3. 普通编号项里再嵌套一个右括号有序列表：
   1) 第一小步
   2) 第二小步

##### 五级标题：无前导管道表格

Key | Value
:--- | ---:
`gamma` | $\gamma$
[spec][spec] | ![标记][diagram]
<span>tag</span> | 普通文本与 &quot;quoted&quot;

###### 六级标题：代码、原始块与收尾

```scheme
(define total
  (fold-left + 0 '(1 2 3 4)))
```

    这里是缩进代码块
    第二行继续

<!-- integration-12 block comment -->

总结小节
--------

最终段落再次引用[文档][doc]、放入自动链接 <https://example.com/final>，并以图片 ![结尾图](ending.png "Ending") 收尾。

[spec]: https://example.com/spec "Spec"
[diagram]: diagram.png "Diagram"
[doc]: https://example.com/doc "Doc"
