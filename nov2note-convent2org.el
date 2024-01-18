;; -*- lexical-binding: t; -*-
(require 'ol)
(require 'dash)

(defun nov2note-convert2org-get-attach-dir ()
  (let* ((data-dir (expand-file-name "data" nov2note-directory))
         (attach-dir (expand-file-name (nov2note--get-note-file-name)
                                       data-dir)))
    (mkdir attach-dir t)
    (file-name-as-directory attach-dir)))

(defun nov2note-convent2org--hanlder-image (content &optional start-pos)
  (let* ((start-pos (or start-pos 0))
         (display-start-pos (next-single-property-change start-pos 'display content)))
    (if display-start-pos
        (let* ((display-end-pos (next-single-property-change display-start-pos 'display content))
               (display-properties (get-char-property display-start-pos 'display content))
               (image-file (image-property display-properties :file)))
          (if image-file
              (let* ((org-attach-file (expand-file-name (file-name-nondirectory image-file)
                                                        (nov2note-convert2org-get-attach-dir)))
                     (_ (copy-file image-file org-attach-file))
                     (image-title (substring-no-properties content display-start-pos display-end-pos))
                     (org-image-link (org-link-make-string (concat "file:" org-attach-file) image-title))
                     (head (substring content 0 display-start-pos))
                     (tail (substring content display-end-pos))
                     (content (concat head org-image-link tail)))
                (nov2note-convent2org--hanlder-image content display-start-pos))
            content))
      content)))

(defun nov2note-convent2org--hanlder-url (content &optional start-pos)
  (let* ((start-pos (or start-pos 0))
         (url-start-pos (next-single-property-change start-pos 'shr-url content)))
    (if url-start-pos
        (let* ((url-end-pos (next-single-property-change url-start-pos 'shr-url content))
               (url-link (get-char-property url-start-pos 'shr-url content))
               (url-title (substring-no-properties content url-start-pos url-end-pos))
               (org-url-link (org-link-make-string url-link url-title))
               (head (substring content 0 url-start-pos))
               (tail (substring content url-end-pos))
               (content (concat head org-url-link tail)))
          (nov2note-convent2org--hanlder-url content url-start-pos))
      content)))

(defvar nov2note-convent2org-face-handers '((bold . (lambda (x)
                                                      (format " *%s* " x)))
                                            (shr-h1 . (lambda (x)
                                                      (format "** %s" x)))
                                            (shr-h2 . (lambda (x)
                                                      (format "*** %s" x)))
                                            (shr-h3 . (lambda (x)
                                                      (format "**** %s" x)))))

(defun nov2note-convent2org--handler-face (content &optional start-pos)
  (let* ((start-pos (or start-pos 0))
         (tag-start-pos (next-single-property-change start-pos 'face content)))
    (if tag-start-pos
        (let* ((tag-end-pos (next-single-property-change tag-start-pos 'face content))
               (face-propertes (get-pos-property tag-start-pos 'face content))
               (middle (substring-no-properties content tag-start-pos tag-end-pos))
               (head (substring content 0 tag-start-pos))
               (tail (if tag-end-pos
                         (substring content tag-end-pos)
                       "")))
          (unless (listp face-propertes)
            (setq face-propertes (list face-propertes)))
          (dolist (property face-propertes)
            (let ((face-handler-fn (alist-get property nov2note-convent2org-face-handers)))
              (if (functionp face-handler-fn)
                  (setq middle (funcall face-handler-fn middle))
                middle)))
          (nov2note-convent2org--handler-face (concat head middle tail) tag-start-pos))
      content)))

(defun nov2note-convent2org--handler-stars (content)
  "remove headline stars(*) which is symbol of headline in org-mode."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (while (re-search-forward "^* " nil t)
      (replace-match "+ "))
    (buffer-string)))

;;;###autoload
(defun nov2note-convent2org (content)
  (-> content
      nov2note-convent2org--handler-face
      nov2note-convent2org--handler-stars
      nov2note-convent2org--hanlder-image
      nov2note-convent2org--hanlder-url))

;; provide feature
(provide 'nov2note-convent2org)
