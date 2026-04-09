# nov2note.el

nov2note.el 是一个 Emacs 包，它将 EPUB 阅读体验与 Org-mode 笔记系统无缝集成。基于优秀的 `nov.el` EPUB 阅读器，nov2note 让你在阅读电子书时可以轻松捕捉高亮和选区内容，自动同步到结构化的 Org 笔记文件中。

## 功能特点

- **自动创建笔记文件**：根据 EPUB 元数据自动生成对应的 Org 笔记文件
- **保留书籍结构**：解析 EPUB 目录（NCX），在 Org 文件中创建对应的层级结构
- **双向链接**：通过 `NOV2NOTE_ID` 属性建立 EPUB 位置与 Org 标题的关联
- **内容格式转换**：自动将 EPUB 中的图片、链接、格式转换为 Org 语法
- **无缝集成**：使用 `org-capture` 插入笔记，体验流畅

## 安装

将 `nov2note.el` 和 `nov2note-convent2org.el` 放入你的 Emacs `load-path` 中，然后在配置文件中添加：

```elisp
(require 'nov2note)
```

## 使用方法

### 基本使用

1. 使用 `nov.el` 打开一个 EPUB 文件
2. 在阅读过程中，选中你想记录的文字内容
3. 调用 `M-x nov2note-capture`
4. 笔记会被插入到对应章节的标题下

### 配置选项

```elisp
;; 设置笔记保存目录（默认：~/Reading）
(setq nov2note-directory "~/Documents/ReadingNotes")

;; 自定义 capture 模板
(setq nov2note-capture-template
      "n %U %^{标题}\n")
```

## 工作原理

当你调用 `nov2note-capture` 时：

1. **获取当前位置**：从 EPUB buffer 获取当前章节的 ID
2. **解析目录结构**：读取 EPUB 的 NCX 文件，构建 Org 标题层级
3. **创建/打开笔记文件**：根据书籍元数据生成文件名（如《书名.org》）
4. **定位对应标题**：使用 `nov2note-find-the-location()` 查找或创建对应的 Org 标题
5. **内容转换**：将选中内容中的图片、链接、格式转换为 Org 语法
6. **插入笔记**：通过 `org-capture` 在当前位置插入笔记

## 依赖

- `nov` - EPUB 阅读器
- `org` - Org-mode
- `xml` - XML 解析
- `xml-query` - XML 查询工具
- `dash` - 现代 Emacs Lisp 工具库

## 项目结构

```
nov2note.el              # 主程序：导航、定位、capture 集成
nov2note-convent2org.el  # 内容转换模块：HTML → Org 格式
```

## 贡献

欢迎提交 issue 和 pull request！

## 许可证

本项目采用与 Emacs 相同的许可证（GPLv3）。
