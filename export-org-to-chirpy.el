;;; export-org-to-chirpy.el --- Export Org posts to Chirpy-ready Markdown -*- lexical-binding: t; -*-

;; Paths — adjust as needed
(setq posts-src "_posts_org")
(setq posts-out "_posts")
(setq assets-dir "assets")

(require 'org)
(require 'subr-x)

;; Escape quotes for YAML
(defun sanitize-yaml (text)
  "Escape double quotes in TEXT for YAML."
  (replace-regexp-in-string "\"" "\\\\\"" (or text "")))

;; Safe helper to run org-element-map and return a string (or empty)
(defun org-get-keyword-value (key)
  "Return the value of org KEYWORD in the current buffer as a string, or \"\"."
  (let ((val (org-element-map (org-element-parse-buffer) 'keyword
               (lambda (k)
                 (when (string= (org-element-property :key k) key)
                   (string-trim (org-element-property :value k))))
               nil t)))
    (cond
      ((stringp val) val)
      ((and (listp val) (stringp (car val))) (car val))
      (t ""))))

;; Read metadata from file (safe)
(defun org-get-metadata (file)
  "Return an alist of Org keywords from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    `((TITLE . ,(org-get-keyword-value "TITLE"))
       (DATE  . ,(org-get-keyword-value "DATE"))
       (TAGS  . ,(org-get-keyword-value "TAGS"))
       (CATEGORIES . ,(org-get-keyword-value "CATEGORIES"))
       (IMAGE_PATH . ,(org-get-keyword-value "IMAGE_PATH"))
       (IMAGE_ALT . ,(org-get-keyword-value "IMAGE_ALT")))))

;; Convert one Org file to Markdown for Jekyll
(defun org-post-to-markdown (org-file output-dir)
  "Export ORG-FILE to Markdown in OUTPUT-DIR, suitable for Jekyll.
- Builds YAML front-matter from keywords
- Removes top #+KEY: lines
- Converts [[file:...][Alt]] and [[file:...]] to Markdown images
- Converts [[URL][TEXT]] -> [TEXT](URL)
- Converts [[URL]] -> <URL>
- Converts src blocks to fenced code blocks
- Converts LaTeX math \\(...\\)/\\[...\\]"
  (let* ((meta (org-get-metadata org-file))
          (title (alist-get 'TITLE meta))
          (date  (alist-get 'DATE meta))
          (tags  (alist-get 'TAGS meta))
          (categories (alist-get 'CATEGORIES meta))
          (image-path (alist-get 'IMAGE_PATH meta))
          (image-alt  (alist-get 'IMAGE_ALT meta)))
    (with-temp-buffer
      (insert-file-contents org-file)
      (org-mode)

      ;; 1) Remove top Org keyword lines so they don't appear in output
      (goto-char (point-min))
      (while (looking-at-p "^#\\+\\(TITLE\\|DATE\\|TAGS\\|CATEGORIES\\|IMAGE_PATH\\|IMAGE_ALT\\):")
        (delete-region (line-beginning-position) (1+ (line-end-position))))

      ;; 2) Convert file links to Markdown images
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[file:\\([^]\n]+\\)\\]\\(?:\\[\\([^]\n]+\\)\\]\\)?" nil t)
        (let* ((path (match-string 1))
                (alt (match-string 2))
                (alt-text (if (and alt (not (string= alt ""))) alt (file-name-base path)))
                (md (format "![%s](%s) " (string-trim alt-text) path)))
          (replace-match md t t)))

      ;; 3) Convert regular Org links [[URL][TEXT]] => [TEXT](URL)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\(https?://[^]\n]+\\)\\]\\[\\([^]\n]+\\)\\]\\]" nil t)
        (let ((url (match-string 1))
               (text (match-string 2)))
          (replace-match (format "[%s](%s)" text url) t t)))

      ;; 4) Convert bracketed URL [[https://...]] => <https://...>
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\(https?://[^]\n]+\\)\\]\\]" nil t)
        (let ((url (match-string 1)))
          (replace-match (format "<%s>" url) t t)))

      ;; 5) Replace org src blocks with fenced code, trim only the last empty line
      (let* ((parsed (org-element-parse-buffer))
              (repls nil))  ;; initialize properly

        ;; Collect all source blocks
        (org-element-map parsed 'src-block
          (lambda (src)
            (let* ((beg (org-element-property :begin src))
                    (end (org-element-property :end src))
                    (lang (string-trim (or (org-element-property :language src) "")))
                    (code (org-element-property :value src))
                    ;; remove exactly one trailing newline if present
                    (code (replace-regexp-in-string "\n\\'" "" code))
                    (fenced (concat "```" lang "\n" code "\n```\n")))
              (setq repls (cons (list beg end fenced) repls)))))

        ;; Sort blocks in reverse buffer order
        (setq repls (sort repls #'(lambda (a b) (> (car a) (car b)))))

        ;; Replace blocks in buffer
        (dolist (rp repls)
          (let ((beg (nth 0 rp))
                 (end (nth 1 rp))
                 (fenced (nth 2 rp)))
            (save-excursion
              (goto-char beg)
              (delete-region beg end)
              (insert fenced)))))


      ;; 6) Convert LaTeX inline/display math
      (goto-char (point-min))
      (while (re-search-forward "\\\\\\((.+?)\\\\\\)" nil t)
        (replace-match "\\$\\1\\$"))
      (goto-char (point-min))
      (while (re-search-forward "\\\\\\[\\(.+?\\)\\\\\\]" nil t)
        (replace-match "\\$\\$\\1\\$\\$"))

      ;; Remove any standalone "]" that appears after an image or at the start of a line
      (goto-char (point-min))
      (while (re-search-forward "\\(\\s-\\|^\\)\\]\\s-*\n" nil t)
        (replace-match "\n"))

      ;; Convert Org headings (** ***) to Markdown (# ##)
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\) \\(.*\\)$" nil t)
        (let* ((stars (match-string 1))
                (level (length stars))
                (text (match-string 2))
                (md-heading (concat (make-string level ?#) " " text)))
          (replace-match md-heading)))

      ;; 7) Gather final content and write output file with YAML
      (let ((content (buffer-string))
             (yaml (concat
                     "---\n"
                     (format "title: \"%s\"\ndate: %s\ncategories: [%s]\ntags: [%s]\n"
                       (sanitize-yaml title)
                       (or date (format-time-string "%Y-%m-%d"))
                       (or categories "")
                       (or tags ""))
                     "math: true\n"
                     ;; Only add image block if IMAGE_PATH exists
                     (when (and image-path (not (string= image-path "")))
                       (format "image:\n  path: %s\n  alt: \"%s\"\n"
                         image-path
                         (sanitize-yaml (or image-alt (file-name-base image-path)))))
                     "---\n\n")))
        (make-directory output-dir t)
        (let ((out-file (expand-file-name (concat (file-name-base org-file) ".md") output-dir)))
          (with-temp-file out-file
            (insert yaml)
            (insert content)))))))

(defun export-org-to-chirpy (posts-src posts-out assets-dir)
  "Export all Org posts (in POSTS-SRC) to Markdown in POSTS-OUT."
  (dolist (f (directory-files posts-src t "\\.org$"))
    (org-post-to-markdown f posts-out)))

;; Run export
(export-org-to-chirpy posts-src posts-out assets-dir)

(message "✅ Org → Chirpy Markdown export complete!")
