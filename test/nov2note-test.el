;; -*- lexical-binding: t; -*-
;;; nov2note-test.el --- Unit tests for nov2note.el -*- lexical-binding: t; -*-

;; Run tests from within Emacs:
;;   M-x load-file RET test/nov2note-test.el RET
;;   M-x ert RET t RET
;;
;; Or in batch mode (from project root):
;;   emacs -batch -L . -l test/nov2note-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'nov2note)

;;;; ID Construction

(ert-deftest nov2note-test-construct-id-with-target ()
  "Test ID construction with filename and target."
  (let ((epub "/tmp/book.epub"))
    (should (equal (nov2note--construct-id epub "chapter1.html" "sec1")
                   (format "%s:chapter1.html#sec1" (md5 epub))))))

(ert-deftest nov2note-test-construct-id-without-target ()
  "Test ID construction without target."
  (let ((epub "/tmp/book.epub"))
    (should (equal (nov2note--construct-id epub "chapter1.html" nil)
                   (format "%s:chapter1.html" (md5 epub))))))

(ert-deftest nov2note-test-construct-id-different-epub ()
  "Test that different EPUB files produce different IDs."
  (let ((id1 (nov2note--construct-id "/tmp/book1.epub" "ch.html" "s1"))
        (id2 (nov2note--construct-id "/tmp/book2.epub" "ch.html" "s1")))
    (should-not (equal id1 id2))))

;;;; Filename Resolution

(ert-deftest nov2note-test-get-current-filename ()
  "Test computing relative filename from NCX directory."
  (let* ((dir (expand-file-name "OEBPS/" temporary-file-directory))
         (nov-toc-id 'ncx)
         (nov-documents (vector (cons 'ncx (expand-file-name "toc.ncx" dir))
                                (cons 'ch1 (expand-file-name "chapter1.html" dir))))
         (nov-documents-index 1))
    (should (equal (nov2note--get-current-filename) "chapter1.html"))))

(ert-deftest nov2note-test-get-current-filename-subdir ()
  "Test relative filename with content in subdirectory."
  (let* ((dir (expand-file-name "OEBPS/" temporary-file-directory))
         (nov-toc-id 'ncx)
         (nov-documents (vector (cons 'ncx (expand-file-name "toc.ncx" dir))
                                (cons 'ch1 (expand-file-name "text/chapter1.html" dir))))
         (nov-documents-index 1))
    (should (equal (nov2note--get-current-filename) "text/chapter1.html"))))

(ert-deftest nov2note-test-get-current-filename-different-index ()
  "Test that different document indices return different filenames."
  (let* ((dir (expand-file-name "OEBPS/" temporary-file-directory))
         (nov-toc-id 'ncx)
         (nov-documents (vector (cons 'ncx (expand-file-name "toc.ncx" dir))
                                (cons 'ch1 (expand-file-name "chapter1.html" dir))
                                (cons 'ch2 (expand-file-name "chapter2.html" dir)))))
    (let ((nov-documents-index 1))
      (should (equal (nov2note--get-current-filename) "chapter1.html")))
    (let ((nov-documents-index 2))
      (should (equal (nov2note--get-current-filename) "chapter2.html")))))

;;;; Note File Name

(ert-deftest nov2note-test-get-note-file-name ()
  "Test note file name uses EPUB title with slashes replaced."
  (let ((nov-metadata '((title . "My/Book")))
        (nov-file-name "/tmp/book.epub"))
    (should (equal (nov2note--get-note-file-name) "My-Book.org"))))

(ert-deftest nov2note-test-get-note-file-name-normal ()
  "Test note file name with normal title."
  (let ((nov-metadata '((title . "Hello World")))
        (nov-file-name "/tmp/book.epub"))
    (should (equal (nov2note--get-note-file-name) "Hello World.org"))))

;;;; Find Nearest Heading ID

(ert-deftest nov2note-test-find-nearest-heading-id-fallback ()
  "Current page has no NCX entry; should find nearest preceding entry."
  (let* ((dir (expand-file-name "OEBPS/" temporary-file-directory))
         (epub "/tmp/test.epub")
         (nov-file-name epub)
         (nov-toc-id 'ncx)
         (nov-documents (vector (cons 'ncx (expand-file-name "toc.ncx" dir))
                                (cons 'ch1 (expand-file-name "text00003.html" dir))
                                (cons 'cont (expand-file-name "text00004.html" dir))
                                (cons 'ch2 (expand-file-name "text00006.html" dir))))
         (nov-documents-index 2)    ; text00004.html - not in alist
         (nov2note-filename-heading-target-alist
          '(("text00003.html" "id_Toc1")
            ("text00006.html" "id_Toc2"))))
    (should (equal (nov2note--find-nearest-heading-id)
                   (nov2note--construct-id epub "text00003.html" "id_Toc1")))))

(ert-deftest nov2note-test-find-nearest-heading-id-no-preceding ()
  "No preceding NCX entry exists; should return nil."
  (let* ((dir (expand-file-name "OEBPS/" temporary-file-directory))
         (nov-file-name "/tmp/test.epub")
         (nov-toc-id 'ncx)
         (nov-documents (vector (cons 'ncx (expand-file-name "toc.ncx" dir))
                                (cons 'intro (expand-file-name "text00001.html" dir))
                                (cons 'ch1 (expand-file-name "text00003.html" dir))))
         (nov-documents-index 1)    ; text00001.html - no preceding entry with alist
         (nov2note-filename-heading-target-alist
          '(("text00003.html" "id_Toc1"))))
    (should (equal (nov2note--find-nearest-heading-id) nil))))

(ert-deftest nov2note-test-find-nearest-heading-id-current-in-alist ()
  "Current page IS in alist; should return its own entry."
  (let* ((dir (expand-file-name "OEBPS/" temporary-file-directory))
         (epub "/tmp/test.epub")
         (nov-file-name epub)
         (nov-toc-id 'ncx)
         (nov-documents (vector (cons 'ncx (expand-file-name "toc.ncx" dir))
                                (cons 'ch1 (expand-file-name "text00003.html" dir))))
         (nov-documents-index 1)
         (nov2note-filename-heading-target-alist
          '(("text00003.html" "id_Toc1"))))
    (should (equal (nov2note--find-nearest-heading-id)
                   (nov2note--construct-id epub "text00003.html" "id_Toc1")))))

(ert-deftest nov2note-test-find-nearest-heading-id-multiple-gaps ()
  "Multiple pages without NCX entries; should skip them all."
  (let* ((dir (expand-file-name "OEBPS/" temporary-file-directory))
         (epub "/tmp/test.epub")
         (nov-file-name epub)
         (nov-toc-id 'ncx)
         (nov-documents (vector (cons 'ncx (expand-file-name "toc.ncx" dir))
                                (cons 'ch1 (expand-file-name "text00003.html" dir))
                                (cons 'c1a (expand-file-name "text00004.html" dir))
                                (cons 'c1b (expand-file-name "text00005.html" dir))
                                (cons 'ch2 (expand-file-name "text00006.html" dir))))
         (nov-documents-index 3)    ; text00005.html - 2 pages after text00003
         (nov2note-filename-heading-target-alist
          '(("text00003.html" "id_Toc1")
            ("text00006.html" "id_Toc2"))))
    (should (equal (nov2note--find-nearest-heading-id)
                   (nov2note--construct-id epub "text00003.html" "id_Toc1")))))

;;;; Get Current Heading ID (with fallback)

(ert-deftest nov2note-test-get-current-heading-id-fallback ()
  "When no shr-target-id found, should fall back to nearest NCX entry."
  (let* ((dir (expand-file-name "OEBPS/" temporary-file-directory))
         (epub "/tmp/test.epub")
         (nov-file-name epub)
         (nov-toc-id 'ncx)
         (nov-documents (vector (cons 'ncx (expand-file-name "toc.ncx" dir))
                                (cons 'ch1 (expand-file-name "text00003.html" dir))
                                (cons 'cont (expand-file-name "text00004.html" dir))
                                (cons 'ch2 (expand-file-name "text00006.html" dir))))
         (nov-documents-index 2)
         (nov2note-filename-heading-target-alist
          '(("text00003.html" "id_Toc1")
            ("text00006.html" "id_Toc2"))))
    (with-temp-buffer
      ;; Empty buffer: no shr-target-id properties, so fallback kicks in
      (should (equal (nov2note--get-current-heading-id)
                     (nov2note--construct-id epub "text00003.html" "id_Toc1"))))))

(ert-deftest nov2note-test-get-current-heading-id-with-target ()
  "When shr-target-id matches a registered target, use it directly."
  (let* ((dir (expand-file-name "OEBPS/" temporary-file-directory))
         (epub "/tmp/test.epub")
         (nov-file-name epub)
         (nov-toc-id 'ncx)
         (nov-documents (vector (cons 'ncx (expand-file-name "toc.ncx" dir))
                                (cons 'ch1 (expand-file-name "text00003.html" dir))))
         (nov-documents-index 1)
         (nov2note-filename-heading-target-alist
          '(("text00003.html" "id_Toc1"))))
    (with-temp-buffer
      (insert "before ")
      (let ((start (point)))
        (insert "target text")
        (put-text-property start (point) 'shr-target-id '("id_Toc1")))
      (goto-char (point-max))
      (should (equal (nov2note--get-current-heading-id)
                     (nov2note--construct-id epub "text00003.html" "id_Toc1"))))))

(ert-deftest nov2note-test-get-current-heading-id-target-not-registered ()
  "When shr-target-id doesn't match a registered target, fallback is used."
  (let* ((dir (expand-file-name "OEBPS/" temporary-file-directory))
         (epub "/tmp/test.epub")
         (nov-file-name epub)
         (nov-toc-id 'ncx)
         (nov-documents (vector (cons 'ncx (expand-file-name "toc.ncx" dir))
                                (cons 'ch1 (expand-file-name "text00003.html" dir))
                                (cons 'cont (expand-file-name "text00004.html" dir))))
         (nov-documents-index 2)
         (nov2note-filename-heading-target-alist
          '(("text00003.html" "id_Toc1"))))
    (with-temp-buffer
      (insert "before ")
      (let ((start (point)))
        (insert "target text")
        (put-text-property start (point) 'shr-target-id '("unknown_id")))
      (goto-char (point-max))
      ;; unknown_id is not registered for text00004.html, so target search
      ;; fails and fallback finds text00003.html's entry
      (should (equal (nov2note--get-current-heading-id)
                     (nov2note--construct-id epub "text00003.html" "id_Toc1"))))))

;;;; NavPoint to Org Heading

(ert-deftest nov2note-test-navpoint2heading ()
  "Test converting a single navPoint to org heading."
  (let ((nov-file-name "/tmp/test.epub")
        (nov2note-filename-heading-target-alist nil))
    (let* ((xml "<navPoint><navLabel><text>Chapter 1</text></navLabel><content src=\"chapter1.html#ch1\"/></navPoint>")
           (dom (with-temp-buffer (insert xml) (car (xml-parse-region))))
           (result (nov2note-navpoint2heading dom 1)))
      (should (string-match "^\\* Chapter 1" result))
      (should (string-match "NOV2NOTE_ID" result)))))

(ert-deftest nov2note-test-navpoint2heading-nested ()
  "Test nested navPoints produce correct heading levels."
  (let ((nov-file-name "/tmp/test.epub")
        (nov2note-filename-heading-target-alist nil))
    (let* ((xml "<navPoint><navLabel><text>Part 1</text></navLabel><content src=\"part1.html\"/><navPoint><navLabel><text>Chapter 1</text></navLabel><content src=\"chapter1.html#ch1\"/></navPoint></navPoint>")
           (dom (with-temp-buffer (insert xml) (car (xml-parse-region))))
           (result (nov2note-navpoint2heading dom 1)))
      (should (string-match "^\\* Part 1" result))
      (should (string-match "^\\*\\* Chapter 1" result)))))

(ert-deftest nov2note-test-navpoint2heading-populates-alist ()
  "Test that generating headings populates the filename-heading-target alist."
  (let ((nov-file-name "/tmp/test.epub")
        (nov2note-filename-heading-target-alist nil))
    (let* ((xml "<navPoint><navLabel><text>Ch1</text></navLabel><content src=\"chapter1.html#s1\"/></navPoint>")
           (dom (with-temp-buffer (insert xml) (car (xml-parse-region)))))
      (nov2note-navpoint2heading dom 1)
      (should (assoc-string "chapter1.html" nov2note-filename-heading-target-alist))
      (should (member "s1" (assoc-string "chapter1.html" nov2note-filename-heading-target-alist))))))

;;;; Content Conversion

(ert-deftest nov2note-test-handler-stars ()
  "Lines starting with '* ' become '+ '; others unchanged."
  (should (equal (nov2note-convent2org--handler-stars "* Heading") "+ Heading"))
  (should (equal (nov2note-convent2org--handler-stars "Normal text") "Normal text"))
  (should (equal (nov2note-convent2org--handler-stars "  * Not at bol") "  * Not at bol"))
  (should (equal (nov2note-convent2org--handler-stars "*Star mid") "*Star mid"))
  (should (equal (nov2note-convent2org--handler-stars "line1\n* Heading\nline3")
                 "line1\n+ Heading\nline3")))

(ert-deftest nov2note-test-handler-face-bold ()
  "Bold face text is wrapped in org bold markers."
  (let ((content (concat "before "
                         (propertize "bold text" 'face 'bold)
                         " after")))
    (should (string-match " \\*bold text\\* " (nov2note-convent2org--handler-face content)))))

(ert-deftest nov2note-test-handler-face-no-face ()
  "Text without face properties passes through unchanged."
  (let ((content "plain text"))
    (should (equal (nov2note-convent2org--handler-face content) "plain text"))))

(ert-deftest nov2note-test-handler-face-mixed ()
  "Text with mixed face and no-face regions."
  (let ((content (concat "normal "
                         (propertize "bold" 'face 'bold)
                         " more normal")))
    (let ((result (nov2note-convent2org--handler-face content)))
      (should (string-match " \\*bold\\* " result))
      (should (string-match "normal" result)))))

(ert-deftest nov2note-test-handler-url ()
  "URLs are converted to org links."
  (let ((content (concat "before "
                         (propertize "click" 'shr-url "https://example.com")
                         " after")))
    (let ((result (nov2note-convent2org--hanlder-url content)))
      (should (string-match "https://example.com" result))
      (should (string-match "click" result)))))

(ert-deftest nov2note-test-handler-url-no-urls ()
  "Text without URL properties passes through unchanged."
  (should (equal (nov2note-convent2org--hanlder-url "plain text") "plain text")))

(ert-deftest nov2note-test-handler-image-no-images ()
  "Text without image properties passes through unchanged."
  (should (equal (nov2note-convent2org--hanlder-image "plain text") "plain text")))

;;;; EPUB Version Dispatch

(ert-deftest nov2note-test-generate-headings-epub2 ()
  "EPUB2 should dispatch to NCX-based heading generation."
  (let ((nov-epub-version "2.0")
        (called nil))
    (cl-letf (((symbol-function 'nov2note--generate-headings-from-ncx)
               (lambda () (setq called 'ncx) ""))
              ((symbol-function 'nov2note--generate-headings-from-nav)
               (lambda () (setq called 'nav) "")))
      (nov2note-generate-headings)
      (should (eq called 'ncx)))))

(ert-deftest nov2note-test-generate-headings-epub3 ()
  "EPUB3 should dispatch to NAV-based heading generation."
  (let ((nov-epub-version "3.0")
        (called nil))
    (cl-letf (((symbol-function 'nov2note--generate-headings-from-ncx)
               (lambda () (setq called 'ncx) ""))
              ((symbol-function 'nov2note--generate-headings-from-nav)
               (lambda () (setq called 'nav) "")))
      (nov2note-generate-headings)
      (should (eq called 'nav)))))

(provide 'nov2note-test)
