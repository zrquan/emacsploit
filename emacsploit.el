;;; emacsploit.el --- Emacs client for Exploit-DB -*- lexical-binding: t; -*-

;; Copyright (C) 2022 4shen0ne
;;
;; Author: 4shen0ne <https://github.com/zrquan>
;; Maintainer: 4shen0ne <4shen.01@gmail.com>
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Emacs client for Exploit-DB.

;;; Code:

(require 'xml)
(require 'json)

(defconst emacsploit--base-url "https://www.exploit-db.com/?%s")

(defconst emacsploit--exploits-url "https://www.exploit-db.com/exploits/%d")

(defvar emacsploit-supported-langs
  '((python-mode . "py")
    (ruby-mode . "rb")
    (html-mode . "html")
    (text-mode . "txt"))
  "Mapping between major-modes and language class.
Used to select appropriate mode from fetched exploit (based on class).")

(defvar emacsploit-table-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "<return>") 'emacsploit-open-current)
    (define-key map (kbd "<C-return>") 'emacsploit-browse-current)
    map)
  "Keymap for `emacsploit-table-mode'.")

(defun unify-newlines (input-string)
  "Replace CRLF in INPUT-STRING with LF."
  (replace-regexp-in-string "\r\n?\\|\n" "\n" input-string))

(defun emacsploit--decode-entities (html)
  "Decode HTML entities in HTML string."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    (while (re-search-forward "&\\(#[0-9]+\\|[a-zA-Z]+\\);" nil t)
      (replace-match
       (pcase (match-string 1)
         ("lt" "<")
         ("gt" ">")
         ("amp" "&")
         ("quot" "\"")
         ("apos" "'")
         ((pred (string-prefix-p "#")) (char-to-string (string-to-number (substring (match-string 1) 1))))
         (_ (match-string 0)))
       t t))
    (buffer-string)))

(defun emacsploit--retrieve-data (params)
  (let ((url (format emacsploit--base-url params)))
    (url-retrieve-synchronously url)))

(defun emacsploit--retrieve-exploit (id)
  (let* ((url (format emacsploit--exploits-url id)))
    (url-retrieve-synchronously url)))

(defun emacsploit-make-query-string (params)
  "Return a query string constructed from PARAMS."
  (mapconcat (lambda (param)
               (concat (url-hexify-string (car param))
                       "="
                       (url-hexify-string (cdr param))))
             params "&"))

(defun emacsploit-open-current ()
  "Open the currently selected exploit by ID."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (and id (eq major-mode 'emacsploit-table-mode))
      (emacsploit-search-by-id (string-to-number id)))))

(defun emacsploit-browse-current ()
  "Browse the currently selected exploit by ID in browser."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (and id (eq major-mode 'emacsploit-table-mode))
      (emacsploit-browse-by-id (string-to-number id)))))

(define-derived-mode emacsploit-table-mode tabulated-list-mode "Exploit-DB"
  "Major mode for displaying a table of exploits from Exploit-DB."
  (setq tabulated-list-format [("Date" 12 nil)
                               ("Type" 10 nil)
                               ("Title" 0 nil)]
        tabulated-list-padding 2
        tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(defun emacsploit--tabulated-entry (data)
  "Create a tabulated list entry from DATA."
  (let* ((desc (cdr (assoc 'description data)))
         (edb-id (aref desc 0))
         (title (aref desc 1))
         (date (cdr (assoc 'date_published data)))
         (type (cdr (assoc 'type_id data))))
    (list edb-id (vector date type (emacsploit--decode-entities title)))))

(defun emacsploit-search-by-id (edb-id)
  "Fetch and display the exploit code by EDB-ID."
  (interactive "nEDB-ID: ")
  (let* ((exp-buffer-name (format "*exploit %d*" edb-id))
         (exp-buffer (get-buffer exp-buffer-name)))
    (if (bufferp exp-buffer)
        (switch-to-buffer-other-window exp-buffer)
      (progn
        (message "Searching exploit %d..." edb-id)
        (setq exp-buffer (emacsploit--retrieve-exploit edb-id))
        (with-current-buffer exp-buffer
          (rename-buffer exp-buffer-name t)
          (goto-char (point-min))
          (if (re-search-forward
               "<code class=\"language-\\(\\w+\\)\"[^>]*>\\([\0-\377[:nonascii:]]*\\)</code>"
               nil t)
              (let* ((lang (match-string 1))
                     (content (match-string 2))
                     (mode (car (rassoc lang emacsploit-supported-langs))))
                (erase-buffer)
                (insert (emacsploit--decode-entities (unify-newlines content)))
                (goto-char (point-min))
                ;; choose major mode
                (when (fboundp mode)
                  (funcall mode))
                (set-buffer-modified-p nil))
            (message "No code found for exploit %d" edb-id)))
        (switch-to-buffer-other-window exp-buffer)))))

(defun emacsploit-browse-by-id (edb-id)
  "Open the browser to view the exploit page by EDB-ID."
  (interactive "nEDB-ID: ")
  (browse-url (format emacsploit--exploits-url edb-id)))

;;;###autoload
(defun emacsploit-search-by-title (text)
  "Search exploits by title (from TEXT) and display the results in a table."
  (interactive "MTitle: ")
  (let* ((exp-buffer-name (format "*exploit %s*" text))
         (exp-buffer (get-buffer exp-buffer-name))
         (request-params (emacsploit-make-query-string
                          `(("columns[0][data]" . "date_published")
                            ("columns[0][name]" . "date_published")
                            ("columns[1][data]" . "download")
                            ("columns[1][name]" . "download")
                            ("columns[1][searchable]" . "false")
                            ("columns[2][data]" . "application_md5")
                            ("columns[2][name]" . "application_md5")
                            ("columns[3][data]" . "verified")
                            ("columns[3][name]" . "verified")
                            ("columns[4][data]" . "description")
                            ("columns[4][name]" . "description")
                            ("columns[5][data]" . "type_id")
                            ("columns[5][name]" . "type_id")
                            ("columns[6][data]" . "platform_id")
                            ("columns[6][name]" . "platform_id")
                            ("columns[7][data]" . "author_id")
                            ("columns[7][name]" . "author_id")
                            ("columns[7][searchable]" . "false")
                            ("columns[8][data]" . "code")
                            ("columns[8][name]" . "code.code")
                            ("columns[9][data]" . "id")
                            ("columns[9][name]" . "id")
                            ("columns[9][searchable]" . "false")
                            ("order[0][column]" . "9")
                            ("order[0][dir]" . "desc")
                            ("start" . "0")
                            ("length" . "60")
                            ("search[value]" . ,text)
                            ("search[regex]" . "false"))))
         (url-request-extra-headers '(("x-requested-with" . "XMLHttpRequest"))))
    (if (bufferp exp-buffer)
        (switch-to-buffer-other-window exp-buffer)
      (progn
        (message "Searching exploit %s..." text)
        (setq exp-buffer (emacsploit--retrieve-data request-params))
        (with-current-buffer exp-buffer
          (rename-buffer exp-buffer-name t)
          (goto-char url-http-end-of-headers)
          (emacsploit-table-mode)
          (setq tabulated-list-entries
                (mapcar #'emacsploit--tabulated-entry
                        ;; get data object from json response
                        (cdr (assoc 'data (json-read)))))
          (tabulated-list-print)
          (set-buffer-modified-p nil))
        (switch-to-buffer-other-window exp-buffer)))))

;;;###autoload
(defun emacsploit-latest-exploits (n)
  "Fetch and display the latest N exploits from Exploit-DB."
  (interactive "nNumber of latest exploits to fetch: ")
  (let* ((exp-buffer-name (format "*latest-exploits-%d*" n))
         (exp-buffer (get-buffer exp-buffer-name))
         (request-params (emacsploit-make-query-string
                          `(("columns[0][data]" . "date_published")
                            ("columns[0][name]" . "date_published")
                            ("columns[1][data]" . "download")
                            ("columns[1][name]" . "download")
                            ("columns[1][searchable]" . "false")
                            ("columns[2][data]" . "application_md5")
                            ("columns[2][name]" . "application_md5")
                            ("columns[3][data]" . "verified")
                            ("columns[3][name]" . "verified")
                            ("columns[4][data]" . "description")
                            ("columns[4][name]" . "description")
                            ("columns[5][data]" . "type_id")
                            ("columns[5][name]" . "type_id")
                            ("columns[6][data]" . "platform_id")
                            ("columns[6][name]" . "platform_id")
                            ("columns[7][data]" . "author_id")
                            ("columns[7][name]" . "author_id")
                            ("columns[7][searchable]" . "false")
                            ("columns[8][data]" . "code")
                            ("columns[8][name]" . "code.code")
                            ("columns[9][data]" . "id")
                            ("columns[9][name]" . "id")
                            ("columns[9][searchable]" . "false")
                            ("order[0][column]" . "0")  ; Order by date published
                            ("order[0][dir]" . "desc") ; Descending order
                            ("start" . "0")
                            ("length" . ,(number-to-string n))
                            ("search[value]" . "")
                            ("search[regex]" . "false"))))
         (url-request-extra-headers '(("x-requested-with" . "XMLHttpRequest"))))
    (if (bufferp exp-buffer)
        (switch-to-buffer-other-window exp-buffer)
      (progn
        (message "Fetching latest %d exploits..." n)
        (setq exp-buffer (emacsploit--retrieve-data request-params))
        (with-current-buffer exp-buffer
          (rename-buffer exp-buffer-name t)
          (goto-char url-http-end-of-headers)
          (emacsploit-table-mode)
          (setq tabulated-list-entries
                (mapcar #'emacsploit--tabulated-entry
                        (cdr (assoc 'data (json-read)))))
          (tabulated-list-print)
          (set-buffer-modified-p nil))
        (switch-to-buffer-other-window exp-buffer)))))

(provide 'emacsploit)
;;; emacsploit.el ends here
