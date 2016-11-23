;;; sleet-mode.el -- Major mode for editing Sleet files

;;; Commentary:

;;; Code:

(defun sleet-find-indent-token ()
  "Find The first."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\n\\([ \t]+\\)[^ \t]+")
      (match-string 1))))

(defconst sleet-re-identifier "[a-zA-Z$@_][a-zA-Z0-9$_\-]*")
(defconst sleet-re-tag-name (concat "\\(" sleet-re-identifier ":\\)?" sleet-re-identifier))
(defconst sleet-re-class-selector (concat "\\." sleet-re-identifier))
(defconst sleet-re-id-selector (concat "#" sleet-re-identifier))
(defconst sleet-re-inline-indicator "[><:+][ \t]+")
(defconst sleet-re-attribute-key (concat
                                  ;; namespace
                                  "\\(" sleet-re-identifier ":\\)?"

                                  ;; key content
                                  "[^ \t=)]+"))
(defconst sleet-re-attribute-value (concat
                                    ;; + prefix
                                    "[ \t]*\\(+[ \t]*\\)?\\("

                                    ;; quoted string
                                    "[\"'][^\"']+[\"']\\|"

                                    ;; numbers
                                    "[0-9]+\\|"

                                    ;; boolean
                                    "\\(true\\|false\\)\\|"

                                    ;; identifier
                                    "[^ \t\n)+]+\\)"
                                    ))

(defconst sleet-re-whole-line (concat
                               ;; prefix
                               "\\(^[ \t]*\\|" sleet-re-inline-indicator  "\\)"

                               ;; tag name or class selector or id selector
                               "\\(" sleet-re-tag-name "\\)?\\(" sleet-re-class-selector "\\|" sleet-re-id-selector "\\)*"

                               ;; attribute start
                               "[ \t]*\\(([ \t]*\\("

                               ;; attribute key
                               "\\(" sleet-re-attribute-key "[ \t]*=\\)?"

                               ;; attribute value
                               "\\(" sleet-re-attribute-value "\\)+"

                               ;; attribute end
                               "\\)+[ \t]*)\\)?[ \t]*\n*"
                               ))

(defun sleet-re-gen-block (re)
  "return a regexp to match sleet block.
RE is used to match block start to identify what kind of block will be matched."
  (concat
   ;; block indent
   "^\\([ \t]*\\)"
   re
   ;; block always ends up with a trailing DOT
   "\\.[ \t]*\n"
   ;; the following lines which with deeper indent is the content of the block
   "\\(\\1[ \t]+[^\n]*\n?"
   ;; skip empty lines
   "\\|^[ \t]*\n\\)+"
   ))

(defconst sleet-re-any-block (sleet-re-gen-block "[^ \t\n]+")
  "Any line that end with a DOT.")

(defconst sleet-re-comment-block (sleet-re-gen-block "#")
  "Comment block start by #.")

(defconst sleet-re-text-block (sleet-re-gen-block "|")
  "Text block start by |.")

(defun sleet-highlight-whole-line (limit)
  (when (re-search-forward sleet-re-whole-line limit t)
    (message "matched: %s" (match-string 0))
    (message "matched: %s" (match-string 1))
    (message "matched: %s" (match-string 2))
    (message "matched: %s" (match-string 3))
    (message "matched: %s" (match-string 4))
    (message "matched: %s" (match-string 5))
    (message "matched: %s" (match-string 6))
    (message "matched: %s" (match-string 7))
    (message "matched: %s" (match-string 8))
    (message "matched: %s" (match-string 9))
    t))

(defun sleet-highlight-comment-block (limit)
  "Highlight comment block.
LIMIT is feeded by font-lock."
  (when (re-search-forward sleet-re-comment-block limit t)
    (put-text-property (match-beginning 0) (match-end 0) 'face font-lock-comment-face)
    t))

(defun sleet-highlight-text-block (limit)
  "Highlight text block.
LIMIT is feeded by font-lock."
  (when (re-search-forward sleet-re-text-block limit t)
    (put-text-property (match-beginning 0) (match-end 0) 'face font-lock-string-face)
    t))

(defun sleet-font-lock-extend-end-of-region ()
  "This function will be called two way:
1. Once file be opened, Font Lock will seperate it in regions from top to
bottom, and then call this function to ensure the region boundaries.
2. After file be changed, Font Lock will call after-change-hood to ensure the
region boundaries and then call this function to ensure again.
So this function only have to ensure the END of the region."

  (message "extend region: (%s, %s)" font-lock-beg font-lock-end)
  (save-excursion
    ;; move point to the start of the next line
    ;; to ensure the last line will be searched
    (goto-char font-lock-end)
    (message "search start at %s" (point))

    ;; it may have two or more blocks in a single region
    ;; search backward to get the last one
    (when (re-search-backward sleet-re-any-block font-lock-beg t)
      ;; when got a block, test if the end of block is nested in current region
      (let ((end (match-end 0)) (token (match-string 1)))
        (when (and
               (eq end font-lock-end)

               ;; token is the indent of current block
               ;; any following lines have deeper indent are the content of current block
               (re-search-forward (concat "\\(^" token "[ \t]+[^\n]*\n?\\|^[ \t]*\n\\)+") nil t))

          (message "set region end to %s" (match-end 0))
          ;; set font-lock-end to match end
          ;; backward one character to keep the point at the end of current block
          (setq font-lock-end (1- (match-end 0))) t)))))

(defun sleet-font-lock-find-parent (beg)
  "Recursive find parent to determine if current line is content of block.
BEG is the point to start search.
return the start point of the found block otherwise return nil"

  (goto-char beg) (beginning-of-line)

  ;; get the indent of current line
  (when (re-search-forward "^\\([ \t]+\\)[^ \t]+" (line-end-position) t)
    (let ((token-length (1- (length (match-string 1)))))

      ;; backward find the line have less indent
      (when (re-search-backward (concat "^\\([ \t]\\)\\{," (number-to-string token-length) "\\}[^ \t\n]+") nil t)

        (beginning-of-line)
        ;; store the start point
        (let ((result (match-beginning 0)))

          ;; test if the current line ends up with a dot
          (if (re-search-forward "^[ \t]*[^\n]+\\.[ \t]*$" (line-end-position) t)

              ;; return the start point of block
              result

            ;; find the parent of current line
            (sleet-font-lock-find-parent result)))))))

(defun sleet-font-lock-extend-beginning-of-region (beg end old-len)
  "This function is used as after-change-hook.

BEG END OLD-LEN are all feeded by font-lock.
This will return a cons present the matched region."
  (save-excursion
    (let ((result (sleet-font-lock-find-parent beg)))
      (when result
        (message "set region beginning to %s" result)

        ;; use orignial end
        (cons result end)
        ))))

(defconst sleet-font-lock-keywords
  `(("doctype" . font-lock-comment-face)
    ("^#![^\n]*$" . font-lock-comment-face)
    ;; ("^[ \t]*\\([a-zA-Z$@_0-9\-]+\\)" 1 font-lock-type-face)
    ;; ("\\([<>+:]\\)[ \t]+\\([a-zA-Z$@_0-9\-]+\\)" . (
    ;;                                             (1 font-lock-keyword-face t)
    ;;                                             (2 font-lock-type-face t)
    ;;                                             ))
    ;; ("^[ \t]*\\([#.][a-zA-Z$@_0-9\-]+\\)" . font-lock-constant-face)
    ;; (sleet-re-whole-line . font-lock-constant-face)
    sleet-highlight-whole-line
    sleet-highlight-comment-block
    sleet-highlight-text-block
    ))

(defvar sleet-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?# ". 1" table)
    (modify-syntax-entry ?\  ". 2" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\' "w" table)
    table)
  "Syntax table for sleet-mode.")

(define-derived-mode sleet-mode fundamental-mode "Sleet"
  "Major mode for editing sleet files"

  (set-syntax-table sleet-mode-syntax-table)

  (set (make-local-variable 'comment-start) "\\(# \\)")
  (set (make-local-variable 'comment-end) "")

  (set (make-local-variable 'font-lock-multiline) t)
  (setq font-lock-defaults '(sleet-font-lock-keywords))
  ;; (setq font-lock-extend-region-functions '(sleet-font-lock-extend-end-of-region))
  ;; (make-local-variable 'font-lock-extend-region-functions)
  ;; (add-to-list 'font-lock-extend-region-functions 'sleet-font-lock-extend-end-of-region t)
  )

(add-to-list 'auto-mode-alist '("\\.sleet$" . sleet-mode))

(provide 'sleet-mode)

;;; sleet-mode ends here
