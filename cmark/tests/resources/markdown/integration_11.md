综合文档十一：数学、表格与原始片段
====================================

开头段落包含 \[literal brackets\]、数字实体 &#35;、命名实体 &quot;quoted&quot;、行内注释 <!-- note -->、行内公式 $E=mc^2$，以及参考式图片 ![矩阵 $A_{ij}$][matrix]。

$$
\frac{a_1+b^2}{\sqrt{c}}
$$

$$
\int_0^1 x^2\,dx
$$

```math
\begin{pmatrix}a&b\\c&d\end{pmatrix}
```

<!-- block note -->

<?process math-block?>

| Column | Description |
| :--- | :---: |
| `alpha` | $\alpha + \beta$ |
| [spec](https://example.com/spec) | ![图][mini] |

```text
raw fenced content
```

    raw indented content
    second line

> 引用中再说一次：访问 <https://example.com/math>，或者查看[参考资料][paper]。
>
> 第二段继续，包含 <span>HTML</span> 与 `code`。

[matrix]: matrix.png "Matrix"
[mini]: mini.png "Mini"
[paper]: https://example.com/paper "Paper"
