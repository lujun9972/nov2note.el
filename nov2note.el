;; -*- lexical-binding: t; -*-
(require 'nov)
(require 'xml)
(require 'xml-query)
(require 'nov2note-convent2org)

;; 1. 打开 epub 对应的笔记文件
;; 1.1 定义笔记存放的位置
(defvar nov2note-directory (expand-file-name
                            "Reading"
                            (or (bound-and-true-p org-roam-directory)
                                (bound-and-true-p org-directory)
                                "~")))
;; 1.2 笔记文件名默认使用 epub 文件 metada 中的 title,若没有 title,则使用 epub 文件名
(defun nov2note--get-note-file-name ()
  (let ((nov-title (replace-regexp-in-string "/" "-" (alist-get 'title nov-metadata)))
        (epub-file-name (file-name-base nov-file-name)))
    (file-name-with-extension (or nov-title epub-file-name)
                              ".org")))

(defun nov2note-get-note-file-path ()
  (expand-file-name (nov2note--get-note-file-name)
                    nov2note-directory))
;; 2 根据文档结构创建全新的笔记文件
;; 2.1 找出 toc.ncx 文件路径，该文件中存储了 epub 的文档结构
(defun nov2note--get-ncx-path ()
  (let* ((nov-documents-alist (mapcar #'identity nov-documents))
         (ncx-path (alist-get nov-toc-id nov-documents-alist)))
    ncx-path))
;; 2.2 解析 toc.ncx
;; toc.ncx 是一个 xml 文件
(defun nov2note--parse-ncx (&optional ncx-path)
  (let ((ncx-path (or ncx-path
                      (nov2note--get-ncx-path))))
    (with-temp-buffer
      (insert-file-contents ncx-path)
      (xml-parse-region))))

;; =docTitle= 中保存的文档标题
(defun nov2note--get-docTitle-from-ncx (ncx-dom)
  (car (xml-node-children (xml-query '(ncx docTitle text) ncx-dom))))

;; =navMap= 中保存的文档目录结构，其包含了多个 =navPoint= 且每个 navPoint 本身又可能包含多个 navPoint 列表，每个 =navPoint= 都对应的一个笔记文件中的 heading
(defun nov2note--get-navpoints-from-ncx (ncx-dom)
  (let ((navMap (xml-query '(ncx navMap) ncx-dom)))
    (xml-query-all '(navPoint) navMap)))
;; 2.3 将 navpoint 转换为 org heading
;; 使用 navpoint 中的 =navLabel= 中的 text 子标签的内容作为 heading 标题
(defun nov2note--get-label-from-navpoint (navpoint-dom)
  (car (xml-node-children (xml-query '(navLabel text) navpoint-dom))))
;; 结合 navpoint 中的 =网页文件地址= 和epub文件路径组合作为 heading 的 ID
(defun nov2note--construct-id (epub-file-name filename &optional target)
  (let ((prefix (md5 epub-file-name))
        (url (if target
                 (format "%s#%s" filename target)
               filename)))
    (format "%s:%s" prefix url)))

;; 同时要记录heading的 =id=,用于根据 shr-target-id 寻找对应 heading 时不会找错位置。
(defvar-local nov2note-filename-heading-target-alist nil
  "记录各个filename中有效的目录target")

(defun nov2note--get-current-filename ()
  "返回当前nov页面的相对页面地址"
  (let* ((document (cdr (aref nov-documents nov-documents-index)))
         (ncx-path (nov2note--get-ncx-path))
         (ncx-directory (file-name-directory ncx-path))
         (filename (file-relative-name document ncx-directory)))
    filename))

(defun nov2note--get-heading-target-list ()
  "返回当前nov页面中有效的 heading target"
  (assoc-string (nov2note--get-current-filename) nov2note-filename-heading-target-alist))

(defun nov2note--heading-target-p (target)
  "判断`TARGET' 是否为标记标题的 target"
  (member target (nov2note--get-heading-target-list)))

(defun nov2note--generate-id-by-navpoint (dom)
  (let* ((url (xml-get-attribute (xml-query '(content) dom) 'src))
         (url-filename-and-target (nov-url-filename-and-target url))
         (filename (nth 0 url-filename-and-target))
         (target (nth 1 url-filename-and-target)))
    (push target (alist-get filename nov2note-filename-heading-target-alist
                            nil nil #'string=))
    (nov2note--construct-id nov-file-name filename target)))

;; 将 navpoint 转换成 org heading 格式
(defun nov2note-navpoint2heading (dom &optional level)
  (let* ((level (or level 1))
         (heading-level (string-join (make-list level "*")))
         (navLabel (nov2note--get-label-from-navpoint dom))
         (id (nov2note--generate-id-by-navpoint dom))
         (navpoint-sublist (xml-query-all '(navPoint) dom))
         (subheadings (nov2note-navpointlist2headings navpoint-sublist
                                                      (+ 1 level))))
    (with-temp-buffer
      (org-mode)
      (insert (format "%s %s\n" heading-level navLabel))
      (org-set-property "NOV2NOTE_ID" id)
      (insert subheadings)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun nov2note-navpointlist2headings (navpointlist &optional level)
  (if navpointlist
      (let* ((level (or level 1))
             (org-content))
        (dolist (navpoint navpointlist)
          (setq org-content (concat org-content "\n" (nov2note-navpoint2heading navpoint level))))
        org-content)
    ""))
;; 3. 若 epub 对应笔记文档不存在则生成新笔记
;; 3.1 根据 epub 的 ncx 文档中的结构生成笔记框架
(defun nov2note--generate-headings-from-ncx (&optional ncx-path)
  (interactive)
  (let* ((ncx-path (or ncx-path (nov2note--get-ncx-path)))
         (toc (nov2note--parse-ncx ncx-path))
         (navPoint-list (nov2note--get-navpoints-from-ncx toc)))
    (nov2note-navpointlist2headings navPoint-list)))

(defun nov2note-generate-headings ()
  (if (version< nov-epub-version "3.0")
      (nov2note--generate-headings-from-ncx)
    (error "epub3 not support Yet!")))
;; 3.2 打开 epub 对应的笔记文档，若笔记文档不存在则生成新笔记
(defun nov2note-create-note-file ()
  "若 epub 对应的笔记文档不存在则生成新笔记，返回笔记文档路径"
  (interactive)
  (let ((note-file (nov2note-get-note-file-path)))
    (unless (file-exists-p note-file)
      (write-region (nov2note-generate-headings)
                    nil
                    note-file))
    note-file))

(defun nov2note-open-note-file ()
  "打开 epub 对应的笔记文档"
  (interactive)
  (find-file (nov2note-create-note-file)))

;; 4. 将选择的内容添加到笔记文件对应的 heading 中
;; 4.1 获取当前 nov 页面对应的日记文件中的标题ID

;; (defun nov2note--find-next-text-property-location (prop &optional pos)
;;   "找到下一个包含`PROP'属性的文本位置"
;;   (let ((pos (or pos (point))))
;;     (if (get-text-property pos prop)
;;         pos
;;       (next-single-property-change pos prop))))

;; (defun nov2note--find-next-text-property (prop)
;;   "跳到下一个包含`PROP'属性的文本位置"
;;   (let ((pos (nov2note--find-next-text-property-location prop)))
;;     (when pos
;;       (goto-char pos)
;;       (get-text-property pos prop))))

(defun nov2note--find-previous-text-property-location (prop &optional pos)
  "找到上一个包含`PROP'属性的文本位置"
  (let ((pos (or pos (point))))
    (if (get-text-property pos prop)
        pos
      (setq pos (previous-single-property-change pos prop))
      (when pos
        (- pos 1)))))

;; (defun nov2note--find-previous-text-property (prop)
;;   "跳到上一个包含`PROP'属性的文本位置,并返回 `PROP' 的值"
;;   (let ((pos (nov2note--find-previous-text-property-location prop)))
;;     (when pos
;;       (goto-char pos)
;;       (get-text-property pos prop))))

(defun nov2note--find-previous-heading-target (&optional start)
  (let* ((start (or start (point)))
         (pos (nov2note--find-previous-text-property-location 'shr-target-id
                                                              start))
         (target (when pos
                   (car (get-text-property pos 'shr-target-id)))))
    (when target
      (if (nov2note--heading-target-p target)
          target
        (when (> pos 1)
          (nov2note--find-previous-heading-target (- pos 1)))))))


(defun nov2note--get-current-heading-id ()
  "获取当前 nov 页面对应的日记文件中的标题ID"
  (let* ((filename (nov2note--get-current-filename))
         ;; shr 通过 shr-target-id 属性来标记 target
         (target (nov2note--find-previous-heading-target)))
    (nov2note--construct-id nov-file-name filename target)))

(defun nov2note-find-the-location (id)
  "定位到记录笔记的地点"
  (let ((pos (or (org-find-property "NOV2NOTE_ID" id)
                 (point-max))))
    (goto-char pos)
    ;; 跳转到下一个同级 heading 之前
    (if (org-goto-sibling)
        ;; (org-goto-first-child)
        (left-char)
        (org-end-of-subtree))))

;; 4.2 将选择的内容添加到笔记文件对应的 heading 中

;; (defun nov2note ()
;;   (interactive)
;;   (call-interactively #'org-store-link)
;;   (let* ((content (if (region-active-p)
;; 	                    (buffer-substring (region-beginning) (region-end))
;; 	                  (error "请选择要记录的内容")))
;;          (org-content (nov2note-convent2org content))
;;          (id (nov2note--get-current-heading-id)))
;;     (save-window-excursion
;;       (save-mark-and-excursion
;;         (nov2note-open-note-file)
;;         (nov2note-find-the-location id)
;;         (org-newline-and-indent)
;;         (insert org-content)
;;         (newline)
;;         (org-insert-last-stored-link 1)))))

;; 4.3 将选择的内容通过 `org-capture' 添加到笔记文件对应的 heading 中
(defvar nov2note-capture-template '("%i\n%a" :immediate-finish t)
  "捕获内容的模板，语法参见 `org-capture-templates' 中的template部分.")

;;;###autoload
(defun nov2note-capture ()
  (interactive)
  ;; 若没有登记合法的 heading target,那么需要通过 =nov2note--generate-heading-from-ncx= 来重新生成
  (unless nov2note-filename-heading-target-alist
    (nov2note-generate-headings))
  (let* ((id (nov2note--get-current-heading-id))
         (location-finder-fn (lambda ()
                               (nov2note-find-the-location id)))
         (content (if (region-active-p)
	                        (buffer-substring (region-beginning) (region-end))
	                      (error "请选择要记录的内容")))
         (org-content (nov2note-convent2org content))
         (org-capture-templates `(("n" "Note taking" plain
                                   (file+function ,(nov2note-create-note-file)
                                                  ,location-finder-fn)
                                   ,@nov2note-capture-template))))
    (org-capture-string org-content "n")))

;; provide feature
(provide 'nov2note)
