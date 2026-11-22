# 综合文档九：基础语义总览

第一段包含 **强强调**、*弱强调*、`inline code`、$x^2+y^2$、<span>行内HTML</span>、实体 Tom &amp; Jerry、转义 \*不是强调\*、自动链接 <https://example.com/docs> 和邮箱 <user@example.com>。
第二行继续这一段
第三行通过反斜杠换行\
第四行继续说明。

小节标题
---------

这里使用参考式[文档][doc]与参考式图片![图标][logo]，同时再放一个直接链接 [主页](https://example.com/home "首页")。

---

> 引用段落里有 $a+b$ 和 <kbd>Ctrl+C</kbd>。
>
> 第二段里还有 `code` 和 [链接](https://example.com/q)。

<div>
raw html block line 1
raw html block line 2
</div>

```scheme
(define (square x)
  (* x x))
```

    这是缩进代码块
    第二行

| Name | Value |
| --- | ---: |
| `x` | $x^2$ |
| [文档](https://example.com/doc) | <span>ok</span> |

[doc]: https://example.com/reference "参考文档"
[logo]: logo.png "Logo"
