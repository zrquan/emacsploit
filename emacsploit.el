;;; emacsploit.el --- search in exploit-db -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 4shen0ne
;;
;; Author: 4shen0ne <https://github.com/zrquan>
;; Maintainer: 4shen0ne <4shen.01@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  search in exploit-db
;;
;;; Code:


(require 'xml)
(require 'json)

(defvar emacsploit-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "o" 'emacsploit-open-current)
    map))

(defun emacsploit-open-current ()
  (interactive)
  (let ((id (and (eq major-mode 'emacsploit-list-mode)
                 (tabulated-list-get-id))))
    (emacsploit-search-by-id (string-to-number id))))

(define-derived-mode emacsploit-list-mode tabulated-list-mode "Exploit-db"
  (setq tabulated-list-format [("Date" 15 nil) ("Title" 0 nil)]
        tabulated-list-padding 2
        tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(defun emacsploit-tabulated-entry (data)
  (let* ((desc (cdr (assoc 'description data)))
         (edb-id (aref desc 0))
         (title (aref desc 1))
         (date (cdr (assoc 'date_published data))))
    (list edb-id (vector date (emacsploit-decode-entities title)))))

(defun emacsploit-decode-entities (html)
  (with-temp-buffer
    (save-excursion (insert html))
    (xml-parse-string)))

;;;###autoload
(defun emacsploit-search-by-title (text)
  (interactive "MTitle: ")

 (let* ((exp-buffer-name (format "*exploit %s*" text))
        (exp-buffer (get-buffer exp-buffer-name))
        ;; (url-mime-accept-string "application/json, text/javascript, */*; q=0.01")
        (request-params
         (emacsploit-make-query-string
          `( ;; TODO 删除不需要的参数
            ("columns[0][data]" . "date_published")
            ("columns[0][name]" . "date_published")
            ("columns[0][searchable]" . "true")
            ("columns[0][orderable]" . "true")
            ("columns[0][search][value]" . "")
            ("columns[0][search][regex]" . "false")
            ("columns[1][data]" . "download")
            ("columns[1][name]" . "download")
            ("columns[1][searchable]" . "false")
            ("columns[1][orderable]" . "false")
            ("columns[1][search][value]" . "")
            ("columns[1][search][regex]" . "false")
            ("columns[2][data]" . "application_md5")
            ("columns[2][name]" . "application_md5")
            ("columns[2][searchable]" . "true")
            ("columns[2][orderable]" . "false")
            ("columns[2][search][value]" . "")
            ("columns[2][search][regex]" . "false")
            ("columns[3][data]" . "verified")
            ("columns[3][name]" . "verified")
            ("columns[3][searchable]" . "true")
            ("columns[3][orderable]" . "false")
            ("columns[3][search][value]" . "")
            ("columns[3][search][regex]" . "false")
            ("columns[4][data]" . "description")
            ("columns[4][name]" . "description")
            ("columns[4][searchable]" . "true")
            ("columns[4][orderable]" . "false")
            ("columns[4][search][value]" . "")
            ("columns[4][search][regex]" . "false")
            ("columns[5][data]" . "type_id")
            ("columns[5][name]" . "type_id")
            ("columns[5][searchable]" . "true")
            ("columns[5][orderable]" . "false")
            ("columns[5][search][value]" . "")
            ("columns[5][search][regex]" . "false")
            ("columns[6][data]" . "platform_id")
            ("columns[6][name]" . "platform_id")
            ("columns[6][searchable]" . "true")
            ("columns[6][orderable]" . "false")
            ("columns[6][search][value]" . "")
            ("columns[6][search][regex]" . "false")
            ("columns[7][data]" . "author_id")
            ("columns[7][name]" . "author_id")
            ("columns[7][searchable]" . "false")
            ("columns[7][orderable]" . "false")
            ("columns[7][search][value]" . "")
            ("columns[7][search][regex]" . "false")
            ("columns[8][data]" . "code")
            ("columns[8][name]" . "code.code")
            ("columns[8][searchable]" . "true")
            ("columns[8][orderable]" . "true")
            ("columns[8][search][value]" . "")
            ("columns[8][search][regex]" . "false")
            ("columns[9][data]" . "id")
            ("columns[9][name]" . "id")
            ("columns[9][searchable]" . "false")
            ("columns[9][orderable]" . "true")
            ("columns[9][search][value]" . "")
            ("columns[9][search][regex]" . "false")
            ("order[0][column]" . "9")
            ("order[0][dir]" . "desc")
            ("start" . "0")
            ("length" . "99")
            ("search[value]" . ,text)
            ("search[regex]" . "false"))))
        (url-request-extra-headers '(("x-requested-with" . "XMLHttpRequest"))))
   (if (bufferp exp-buffer)
       (switch-to-buffer-other-window exp-buffer)
     (progn
       (message "Searching exploit %s..." text)
       (setq exp-buffer
             (url-retrieve-synchronously (format "https://www.exploit-db.com/?%s" request-params)))
       (with-current-buffer exp-buffer
         (rename-buffer exp-buffer-name t)
         (goto-char url-http-end-of-headers)
         ;; (delete-region (point-min) (point))
         (emacsploit-list-mode)
         (setq tabulated-list-entries
               (mapcar 'emacsploit-tabulated-entry
                       (cdr (assoc 'data (json-read)))))
         (tabulated-list-print)
         (set-buffer-modified-p nil))
       (switch-to-buffer-other-window exp-buffer)))))

(defun emacsploit-make-query-string (params)
  "Return a query string constructed from PARAMS."
  (mapconcat
   (lambda (param)
     (concat (url-hexify-string (car param)) "="
             (url-hexify-string (cdr param))))
   params "&"))

(defun emacsploit-search-by-id (edb-id)
  "Search exp from exploit-db by EDB-ID."
  (interactive "nEDB-ID: ")

  (let* ((exp-buffer-name (format "*exploit %d*" edb-id))
         (exp-buffer (get-buffer exp-buffer-name)))
    (if (bufferp exp-buffer)
        (switch-to-buffer-other-window exp-buffer)
      (progn
        (message "Searching exploit %d..." edb-id)
        (setq exp-buffer
              (url-retrieve-synchronously (format "https://www.exploit-db.com/exploits/%d" edb-id)))
        (with-current-buffer exp-buffer
          (rename-buffer exp-buffer-name t)
          (goto-char (point-min))

          (save-excursion
            (search-forward-regexp "<code[^>]*>")
            (delete-region (point-min) (point))
            (search-forward-regexp "</code>")
            (backward-char 7)
            (delete-region (point) (point-max)))

          (save-excursion (xml-parse-string))

          (save-excursion
            (while (re-search-forward "\r" nil :noerror)
              (replace-match "")))

          (text-mode)
          (set-buffer-modified-p nil))
        (switch-to-buffer-other-window exp-buffer)))))

(provide 'emacsploit)
;;; emacsploit.el ends here
