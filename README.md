# Handoc

> [!WARNING]
> Not a serious implantation. Use [pandoc]() in that case.

A tiny markdown to HTML converter written in Haskell

```sh
make
echo "# Hello, World" | ./Handoc
./Handoc test.md > test.html
```
[pandoc]: https://github.com/jgm/pandoc

### Features supported

- [x] Heading
- [x] Paragraph
- [x] Blockquote
- [x] Bold, Italic
- [x] Horizontal line
- [x] Code snippet
- [x] Simple Ordered/Unordered list
- [ ] Nested Ordered/Unordered list
- [x] Simple hyperlink
- [ ] Reference hyperlink
- [x] Image
- [ ] Table
