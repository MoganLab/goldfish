# 部署流程

开始前请阅读[总文档][]，并确认\# 这一项不是标题。
如果失败，请记录错误代码&#35;42，并查看![安装包][pkg]。

- 准备阶段
  - 阅读[变更记录](https://example.com/changelog)
  - 执行`gf test`
- 执行阶段
  1. 打开控制台
  2. 输入命令  
     然后等待完成
  3. 查看<https://example.com/build>
- 收尾阶段
  - 记录*结果*
  - 标记**完成**

    export PATH=/opt/bin:$PATH
    gf test

> 注意事项
>
> 第一行
> 第二行
>
> 1) 内层步骤
> 2) 内层步骤包含[外部文档][outer]
>
> [outer]: https://example.com/outer "外部文档"

    export PATH=/opt/bin:$PATH
    gf test

[总文档]: https://example.com/docs "总文档"
[pkg]: package.zip "安装包"
