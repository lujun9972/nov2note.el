;; 1. 打开 epub 对应的笔记文件
;; 1.1 定义笔记存放的位置
(defvar nov2note-directory org-directory)
;; (setq nov2note-directory (expand-file-name "Reading" org-roam-directory))
;; 1.2 笔记文件名默认使用 epub 文件 metada 中的 title,若没有 title,则使用 epub 文件名
(defun nov2note--get-note-file-name ()
  (let ((nov-title (alist-get 'title nov-metadata))
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
;; 通过 navpoint 中的 =网页文件地址= 作为 heading 的 ID
(defun nov2note--generate-id-by-navpoint (dom)
  (let ((content (xml-get-attribute (xml-query '(content) dom) 'src)))
    (expand-file-name content nov-temp-dir)))
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
      (org-set-property "ID" id)
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
;; 3.2 打开 epub 对应的笔记文档，若笔记文档不存在则生成新笔记
(defun nov2note-create-note-file ()
  "若 epub 对应的笔记文档不存在则生成新笔记，返回笔记文档路径"
  (interactive)
  (let ((note-file (nov2note-get-note-file-path)))
    (unless (file-exists-p note-file)
      (write-region (nov2note--generate-headings-from-ncx)
                    nil
                    note-file))
    note-file))

(defun nov2note-open-note-file ()
  "打开 epub 对应的笔记文档"
  (interactive)
  (find-file (nov2note-create-note-file)))

;; 4. 将选择的内容添加到笔记文件对应的 heading 中
;; 4.1 获取当前 nov 页面对应的日记文件中的标题ID
(defun nov2note--get-current-heading-id ()
  "获取当前 nov 页面对应的日记文件中的标题ID"
  (cdr (aref nov-documents nov-documents-index)))

;; 4.2 将选择的内容添加到笔记文件对应的 heading 中
(defun nov2note ()
  (interactive)
  (let* ((content (if (region-active-p) 
	                    (buffer-substring-no-properties (region-beginning) (region-end))
	                  (error "请选择要记录的内容")))
         (id (nov2note--get-current-heading-id)))
    (save-window-excursion
      (save-mark-and-excursion
        (nov2note-open-note-file)
        (let ((pos (or (org-find-entry-with-id id)
                       (point-max))))
          (goto-char pos)
          (org-end-of-subtree)
          (org-newline-and-indent)
          (insert content))))))

;; provide feature
(provide 'nov2note)
