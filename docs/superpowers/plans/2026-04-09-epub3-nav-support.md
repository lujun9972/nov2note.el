# EPUB3 NAV Support Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add EPUB3 support by parsing NAV (XHTML navigation) files, enabling TOC generation and note capture for EPUB3 books.

**Architecture:** Add parallel NAV parsing functions alongside existing NCX functions in `nov2note.el`. Dispatch by `nov-epub-version` in `nov2note-generate-headings`. Reuse existing ID generation and note capture mechanisms.

**Tech Stack:** Emacs Lisp, `libxml-parse-html-region` (built-in), `dom-*` functions (built-in), existing `nov.el` variables.

---

## File Structure

- **Modify:** `nov2note.el` — Add 4 new NAV parsing functions + modify `nov2note-generate-headings`
- **No new files** — All changes in the existing module

## Key Insight

`nov2note--get-ncx-path` already works for EPUB3 because it uses `nov-toc-id` + `nov-documents`, and `nov.el` sets `nov-toc-id` to the NAV file's ID for EPUB3 books. Similarly, `nov2note--get-current-filename` uses the TOC file's directory as the base for relative paths — this works for both EPUB2 and EPUB3 since both NCX and NAV files are typically in the same OEBPS directory. **Neither of these functions needs modification.**

---

### Task 1: Add NAV parsing helper functions

**Files:**
- Modify: `nov2note.el` (add new functions after `nov2note--generate-headings-from-ncx`, around line 119)

- [ ] **Step 1: Add `nov2note--parse-nav` function**

Insert after line 119 (after `nov2note--generate-headings-from-ncx`):

```elisp
(defun nov2note--parse-nav (&optional nav-path)
  "Parse NAV file and return DOM tree.
NAV-PATH defaults to the TOC document path from `nov-documents'."
  (let ((nav-path (or nav-path (nov2note--get-ncx-path))))
    (with-temp-buffer
      (insert-file-contents nav-path)
      (libxml-parse-html-region (point-min) (point-max)))))
```

Note: `nov2note--get-ncx-path` returns the TOC path for both EPUB2 and EPUB3, so we reuse it here.

- [ ] **Step 2: Add `nov2note--get-toc-nav-element` function**

```elisp
(defun nov2note--get-toc-nav-element (dom)
  "Find the <nav> element with epub:type=\"toc\" from DOM.
Fallback to first <nav> containing an <ol>."
  (or (dom-search dom
        (lambda (node)
          (and (eq (dom-tag node) 'nav)
               (string= (dom-attr node 'epub:type) "toc"))))
      ;; Fallback: first <nav> with an <ol> child
      (dom-search dom
        (lambda (node)
          (and (eq (dom-tag node) 'nav)
               (seq-some (lambda (child) (eq (dom-tag child) 'ol))
                         (dom-children node)))))))
```

- [ ] **Step 3: Add `nov2note--nav-li2heading` function**

This is the core recursive function that converts a single `<li>` element to an Org heading string. It mirrors `nov2note-navpoint2heading`.

```elisp
(defun nov2note--nav-li2heading (li-element level)
  "Convert a NAV <li> ELEMENT to an Org heading at LEVEL.
If the <li> contains an <a> with href, generate a heading with NOV2NOTE_ID.
If it only has a <span>, generate a heading without ID."
  (let* ((heading-level (string-join (make-list level "*")))
         (a-element (dom-by-tag li-element 'a))
         (span-element (dom-by-tag li-element 'span))
         (has-link (and a-element (> (length a-element) 0))))
    (if has-link
        (let* ((a-node (car a-element))
               (navLabel (dom-text a-node))
               (url (dom-attr a-node 'href))
               (url-filename-and-target (nov-url-filename-and-target url))
               (filename (nth 0 url-filename-and-target))
               (target (nth 1 url-filename-and-target))
               (id (progn
                     (push target (alist-get filename nov2note-filename-heading-target-alist
                                             nil nil #'string=))
                     (nov2note--construct-id nov-file-name filename target))))
          ;; Find nested <ol> within this <li>
          (let* ((nested-ol (seq-filter (lambda (child) (eq (dom-tag child) 'ol))
                                        (dom-children li-element)))
                 (subheadings (if nested-ol
                                  (nov2note--nav-ol2headings (car nested-ol) (+ 1 level))
                                "")))
            (with-temp-buffer
              (org-mode)
              (insert (format "%s %s\n" heading-level navLabel))
              (org-set-property "NOV2NOTE_ID" id)
              (insert subheadings)
              (buffer-substring-no-properties (point-min) (point-max)))))
      ;; No <a> link — use <span> text as heading title without ID
      (let* ((navLabel (if (and span-element (> (length span-element) 0))
                           (dom-text (car span-element))
                         "Untitled"))
             (nested-ol (seq-filter (lambda (child) (eq (dom-tag child) 'ol))
                                    (dom-children li-element)))
             (subheadings (if nested-ol
                              (nov2note--nav-ol2headings (car nested-ol) (+ 1 level))
                            "")))
        (with-temp-buffer
          (org-mode)
          (insert (format "%s %s\n" heading-level navLabel))
          (insert subheadings)
          (buffer-substring-no-properties (point-min) (point-max)))))))
```

- [ ] **Step 4: Add `nov2note--nav-ol2headings` function**

```elisp
(defun nov2note--nav-ol2headings (ol-element level)
  "Convert all <li> children of OL-ELEMENT to Org headings at LEVEL."
  (let ((li-elements (seq-filter (lambda (child) (eq (dom-tag child) 'li))
                                 (dom-children ol-element)))
        (org-content ""))
    (dolist (li li-elements)
      (setq org-content (concat org-content "\n"
                                (nov2note--nav-li2heading li level))))
    org-content))
```

- [ ] **Step 5: Commit**

```bash
git add nov2note.el
git commit -m "feat: add EPUB3 NAV parsing functions"
```

---

### Task 2: Add `nov2note--generate-headings-from-nav` and wire up dispatch

**Files:**
- Modify: `nov2note.el` (add function + modify `nov2note-generate-headings`)

- [ ] **Step 1: Add `nov2note--generate-headings-from-nav` function**

Insert after the NAV parsing helpers:

```elisp
(defun nov2note--generate-headings-from-nav ()
  "Generate Org headings from EPUB3 NAV file."
  (let* ((nav-dom (nov2note--parse-nav))
         (toc-nav (nov2note--get-toc-nav-element nav-dom))
         (toc-ol (when toc-nav
                   (car (seq-filter (lambda (child) (eq (dom-tag child) 'ol))
                                    (dom-children toc-nav))))))
    (if toc-ol
        (nov2note--nav-ol2headings toc-ol 1)
      "")))
```

- [ ] **Step 2: Modify `nov2note-generate-headings` to dispatch to NAV parser**

Change the function at line 121-124 from:

```elisp
(defun nov2note-generate-headings ()
  (if (version< nov-epub-version "3.0")
      (nov2note--generate-headings-from-ncx)
    (error "epub3 not support Yet!")))
```

To:

```elisp
(defun nov2note-generate-headings ()
  (if (version< nov-epub-version "3.0")
      (nov2note--generate-headings-from-ncx)
    (nov2note--generate-headings-from-nav)))
```

- [ ] **Step 3: Commit**

```bash
git add nov2note.el
git commit -m "feat: wire up EPUB3 NAV support in generate-headings"
```

---

### Task 3: Update comments for clarity

**Files:**
- Modify: `nov2note.el`

- [ ] **Step 1: Update comment at line 25 to reflect dual NCX/NAV support**

Change:
```elisp
;; 2.1 找出 toc.ncx 文件路径，该文件中存储了 epub 的文档结构
```

To:
```elisp
;; 2.1 找出 TOC 文件路径（EPUB2 为 toc.ncx，EPUB3 为 nav.xhtml）
```

- [ ] **Step 2: Commit**

```bash
git add nov2note.el
git commit -m "docs: update comments for EPUB2/EPUB3 dual support"
```

---

## Spec Coverage Check

| Spec Requirement | Task |
|---|---|
| `nov2note--parse-nav` | Task 1, Step 1 |
| `nov2note--get-toc-nav-element` | Task 1, Step 2 |
| `nov2note--nav-ol2headings` (core recursive) | Task 1, Steps 3-4 |
| `nov2note--generate-headings-from-nav` | Task 2, Step 1 |
| Modify `nov2note-generate-headings` dispatch | Task 2, Step 2 |
| `nov2note--get-current-filename` modification | **Not needed** — `nov2note--get-ncx-path` already returns NAV path for EPUB3 |
| Handle `<a href>` links | Task 1, Step 3 (has-link branch) |
| Handle `<span>` only items | Task 1, Step 3 (no-link branch) |
| Handle nested `<ol>` | Task 1, Step 3 (recursive call) |
