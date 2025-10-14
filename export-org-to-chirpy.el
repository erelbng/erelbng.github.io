;;; export-org-to-chirpy.el --- Export Org posts to Chirpy-ready HTML -*- lexical-binding: t; -*-

(setq posts-src "_posts_org")
(setq posts-out "_posts")
(setq tabs-src "_tabs_org")
(setq tabs-out "_tabs_temp")
(setq assets-dir "assets")

(require 'org)

;; Escape quotes for YAML
(defun sanitize-yaml (text)
  "Escape double quotes in TEXT for YAML."
  (replace-regexp-in-string "\"" "\\\\\"" text))

(defun org-get-metadata (file)
  "Return an alist of Org keywords (TITLE, DATE, TAGS, CATEGORIES, IMAGE_PATH, IMAGE_ALT) from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((title (or (org-element-map (org-element-parse-buffer) 'keyword
                      (lambda (k) (when (string= (org-element-property :key k) "TITLE")
                                    (org-element-property :value k))) nil t)
                     "Untitled"))
          (date (or (org-element-map (org-element-parse-buffer) 'keyword
                     (lambda (k) (when (string= (org-element-property :key k) "DATE")
                                   (org-element-property :value k))) nil t)
                    (format-time-string "%Y-%m-%d")))
          (tags (or (org-element-map (org-element-parse-buffer) 'keyword
                     (lambda (k) (when (string= (org-element-property :key k) "TAGS")
                                   (org-element-property :value k))) nil t)
                    ""))
          (categories (or (org-element-map (org-element-parse-buffer) 'keyword
                          (lambda (k) (when (string= (org-element-property :key k) "CATEGORIES")
                                        (org-element-property :value k))) nil t)
                        ""))
          (image-path (org-element-map (org-element-parse-buffer) 'keyword
                        (lambda (k) (when (string= (org-element-property :key k) "IMAGE_PATH")
                                      (org-element-property :value k))) nil t))
          (image-alt (org-element-map (org-element-parse-buffer) 'keyword
                       (lambda (k) (when (string= (org-element-property :key k) "IMAGE_ALT")
                                     (org-element-property :value k))) nil t)))
      `((TITLE . ,title)
        (DATE . ,date)
        (TAGS . ,tags)
        (CATEGORIES . ,categories)
        (IMAGE_PATH . ,image-path)
        (IMAGE_ALT . ,image-alt)))))

(defun org-post-to-html (org-file output-dir assets-dir)
  "Export an Org post to Chirpy HTML."
  (let* ((meta (org-get-metadata org-file))
         (title (alist-get 'TITLE meta))
         (date (alist-get 'DATE meta))
         (tags (alist-get 'TAGS meta))
         (categories (alist-get 'CATEGORIES meta))
         (image-path (alist-get 'IMAGE_PATH meta))
         (image-alt (alist-get 'IMAGE_ALT meta))
         (content (with-temp-buffer
                    (insert-file-contents org-file)
                    (org-mode)
                    (let ((html (org-export-as 'html nil nil t nil)))
                      (goto-char (point-min))
                      (while (re-search-forward "\\\\\\((.+?)\\\\\\)" nil t)
                        (replace-match "\\$\\1\\$"))
                      (goto-char (point-min))
                      (while (re-search-forward "\\\\\\[\\(.+?\\)\\\\\\]" nil t)
                        (replace-match "\\$\\$\\1\\$\\$"))
                      html))))
    ;; Build YAML front-matter
    (let ((yaml (concat "---\n"
                        (format "title: \"%s\"\ndate: %s\ncategories: [%s]\ntags: [%s]\n"
                                (sanitize-yaml title) date categories tags))))
      (when image-path
        ;; Provide default alt if missing
        (setq yaml (concat yaml
                           (format "image:\n  path: %s\n  alt: \"%s\"\n"
                                   image-path
                                   (sanitize-yaml (or image-alt (file-name-base image-path)))))))
      (setq yaml (concat yaml "---\n"))
      (make-directory output-dir t)
      (let ((output-file (expand-file-name (concat (file-name-base org-file) ".html") output-dir)))
        (with-temp-buffer
          (insert yaml)
          (insert content)
          (write-region (point-min) (point-max) output-file))))))

(defun export-org-to-chirpy (posts-src posts-out tabs-src tabs-out assets-dir)
  "Export all Org posts and tabs to Chirpy."
  (dolist (file (directory-files posts-src t "\\.org$"))
    (org-post-to-html file posts-out assets-dir)))

(export-org-to-chirpy posts-src posts-out tabs-src tabs-out assets-dir)

(message "✅ Org → Chirpy HTML export complete!")
