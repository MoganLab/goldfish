综合文档十三：引用定义、数学块与原始片段
============================================

开篇段落使用参考式[入口][entry]、参考式图片![封面][cover]、自动链接 <https://example.org/root>、邮箱 <ops@example.org>、行内注释 <!-- inline note -->、行内公式 $E = mc^2$、以及转义 \[方括号\]。

---

> 块引用里先放一个段落，里面包含 [文档][entry]、`command --help`、<span>html</span> 和实体 &#35;。
>
> 接着放一个列表：
>
> - 第一项
> - 第二项里带图片 ![图][inner]
>
> 最后补一个数学块：
>
> $$
> \int_0^1 x^2\,dx
> $$

$$
\begin{aligned}
f(x) &= x^2 + 1 \\
g(x) &= \sqrt{x}
\end{aligned}
$$

```math
\begin{pmatrix}
a & b \\
c & d
\end{pmatrix}
```

<section>
raw html block line 1
raw html block line 2
</section>

<?integration process?>

| Field | Example |
| --- | :--- |
| `alpha` | $\alpha + \beta$ |
| [入口][entry] | ![封面][cover] |
| <span>mini</span> | 普通文本 |

1. 第一编号项。
2. 第二编号项里有 [直接链接](https://example.org/inline "Inline")。
3. 第三编号项里嵌套无序列表：
   - 子项 A
   - 子项 B，包含 $x_i$

结尾段落再次使用参考式[文档][entry]、直接图片 ![尾图](tail.png "Tail")、以及 raw inline <kbd>Esc</kbd>。

[entry]: https://example.org/entry "Entry"
[cover]: cover-ref.png "CoverRef"
[inner]: inner.png "Inner"
