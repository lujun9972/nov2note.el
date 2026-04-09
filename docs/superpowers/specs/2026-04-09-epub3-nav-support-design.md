# EPUB3 NAV Support Design

## Problem

nov2note.el only supports EPUB2 (NCX-based navigation). When opening an EPUB3 file, `nov2note-generate-headings` throws `"epub3 not support Yet!"`. EPUB3 replaces NCX with NAV (XHTML navigation) files, requiring a new parser.

## Approach

Use `libxml-parse-html-region` (Emacs built-in) to parse NAV XHTML files and extract the TOC hierarchy. Add a parallel set of functions to the existing NCX parsing, dispatching by `nov-epub-version`.

## Architecture

```
nov2note-generate-headings
  ├── EPUB2 (< 3.0) → nov2note--generate-headings-from-ncx   (existing)
  └── EPUB3 (>= 3.0) → nov2note--generate-headings-from-nav  (new)
```

## New Functions

All new functions go in `nov2note.el`:

### `nov2note--get-nav-path`
Get the NAV file path. Reuses `nov-toc-id` + `nov-documents` (same pattern as `nov2note--get-ncx-path`).

### `nov2note--parse-nav`
Parse the NAV file using `libxml-parse-html-region`, returning a DOM tree.

### `nov2note--get-toc-nav-element`
Find the `<nav>` element with `epub:type="toc"` attribute from the DOM. Fallback: find the first `<nav>` containing an `<ol>`.

### `nov2note--nav-ol2headings`
Core recursive function. Walks `<ol>/<li>` structure and generates Org headings:
- **`<li>` with `<a href>`**: Extract href and text as heading title, generate Org heading with NOV2NOTE_ID property
- **`<li>` with only `<span>` (no `<a>`)**: Extract span text as heading title, generate Org heading without ID (no corresponding page)
- **`<li>` with nested `<ol>`**: Recurse with level+1

Output format matches existing `nov2note-navpoint2heading` exactly.

## Modified Functions

### `nov2note-generate-headings` (line 121-124)
Change from:
```elisp
(if (version< nov-epub-version "3.0")
    (nov2note--generate-headings-from-ncx)
  (error "epub3 not support Yet!"))
```
To:
```elisp
(if (version< nov-epub-version "3.0")
    (nov2note--generate-headings-from-ncx)
  (nov2note--generate-headings-from-nav))
```

### `nov2note--get-current-filename` (line 63-69)
Currently uses NCX path to compute relative filenames. For EPUB3, use NAV path as the base directory instead. Dispatch by `nov-epub-version`.

## Dependencies

- `libxml-parse-html-region`: Built into Emacs when compiled with libxml2 support (standard in most distributions). No new package dependencies.

## Scope

Minimum viable support: TOC generation and note capture for EPUB3 files. Handles standard NAV structures with `<a href>` links and nested `<ol>` hierarchies.
