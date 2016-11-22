;;; sleet-mode.el -- Major mode for editing Sleet files

;;; Commentary:

;;; Code:

(defun sleet-find-indent-token ()
  "Find The first."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\n\\([ \t]+\\)[^ \t]+")
      (match-string 1))))

(defun sleet-get-block-region ()
  "Set font-lock-beg and font-lock-end to current block."
  (beginning-of-line)
  (when (re-search-forward "^\\([ \t]+\\)[^ \t\n]+[^\n]*\\.[ \t]*\n\\(\\1[ \t]+[^\n]*\n?\\|^[ \t]*\n\\)+" nil t)
    (cons (match-beginning 0) (1- (match-end 0)))))

(defun sleet-font-lock-region-backward (beg)
  "Find the nearest parent line, and call the matcher.
MATCHER is used to test if the current if a block starter."

  (goto-char beg)
  (beginning-of-line)
  (when (re-search-forward "^\\([ \t]+\\)[^ \t]+" (line-end-position) t)
    (let ((tl (1- (length (match-string 1)))))
      (when (re-search-backward (concat "^\\([ \t]\\)\\{," (number-to-string tl) "\\}[^ \t\n]+") nil t)
        (beginning-of-line)
        (let ((bb (match-beginning 0)))
        (if (re-search-forward "^[ \t]*[^\n]+\\.[ \t]*$" (line-end-position) t)
            (sleet-get-block-region)
          (sleet-font-lock-region-backward bb)))))))

(defun sleet-font-lock-region-forward ()
  "Look forward to enlarge font lock region."
  (goto-char (1+ font-lock-end))
  (beginning-of-line)
  (let ((pp font-lock-beg))
    (when (re-search-backward "^\\([ \t]+\\)[^ \t\n]+[^\n]*\\.[ \t]*\n\\(\\1[ \t]+[^\n]*\n?\\|^[ \t]*\n\\)+" font-lock-beg t)
      (setq font-lock-beg (min pp (match-beginning 0)))
      (message "forward start set to [%s]" font-lock-beg)
      (let ((ee (match-end 0)) (token (match-string 1)))
        (when (and (eq ee font-lock-end) (re-search-forward (concat "\\(^" token "[ \t]+[^\n]*\n?\\|^[ \t]*\n\\)+") nil t))
          (setq font-lock-end (max font-lock-end (1- (match-end 0))))
          (message "forward end set to [%s]" font-lock-end) t)))))

(defun sleet-font-lock-region-start-from-beginning ()
  "Each region should start from beginning of line."
  (goto-char font-lock-beg)
  (let ((pp (point)))
    (beginning-of-line)
    (when (/= pp (point))
      (message "beginning [%s]" (min pp (point)))
      (setq font-lock-beg (min pp (point))) t)))

(defun sleet-extend-font-lock-region ()
  "Function that used to unsure font lock region."
  (message "extend region from [%s, %s]" font-lock-beg font-lock-end)
  (save-excursion
    (or
     (sleet-font-lock-region-forward)
     (sleet-font-lock-region-start-from-beginning)
     )))

(defun sleet-font-lock-after-change (beg end old-len)
  "Get the correct region after changed"
  (message "after change [%s, %s, %s]" beg end old-len)
  (save-excursion
    (let ((result (sleet-font-lock-region-backward beg)))
      (when result
        (message "result [%s]" result) result))))

(defun sleet-highlight-comment-block (limit)
  "Highlight comment block.  LIMIT is feeded by font-lock."
  (when (re-search-forward "\\([ \t]*\\)#\\.[ \t]*\n\\(\\1[ \t]+[^\n]*\n?\\|^[ \t]*\n\\)+" limit t)
    (put-text-property (match-beginning 0) (match-end 0) 'face font-lock-comment-face)
    t))

(defconst sleet-font-lock-keywords
  `(("doctype" . font-lock-comment-face)
    ("^#![^\n]*$" . font-lock-comment-face)
    ("^[ \t]*\\([a-zA-Z$@_0-9\-]+\\)" 1 font-lock-type-face)
    ("\\([<>+:]\\)[ \t]+\\([a-zA-Z$@_0-9\-]+\\)" . (
                                                (1 font-lock-keyword-face t)
                                                (2 font-lock-type-face t)
                                                ))
    ("^[ \t]*\\([#.][a-zA-Z$@_0-9\-]+\\)" . font-lock-constant-face)
    sleet-highlight-comment-block
    ))

(defvar sleet-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?# ". 1" table)
    (modify-syntax-entry ?\  ". 2" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\' "\"" table)
    table)
  "Syntax table for sleet-mode.")

(define-derived-mode sleet-mode fundamental-mode "Sleet"
  "Major mode for editing sleet files"

  (set-syntax-table sleet-mode-syntax-table)

  (set (make-local-variable 'comment-start) "\\(# \\)")
  (set (make-local-variable 'comment-end) "")

  (set (make-local-variable 'font-lock-multiline) t)
  (setq font-lock-defaults '(sleet-font-lock-keywords))
  (make-local-variable 'font-lock-extend-region-functions)
  (add-hook 'font-lock-extend-region-functions 'sleet-extend-font-lock-region)
  (setq font-lock-extend-after-change-region-function 'sleet-font-lock-after-change)
  )

(add-to-list 'auto-mode-alist '("\\.sleet$" . sleet-mode))

(provide 'sleet-mode)

;;; sleet-mode ends here
