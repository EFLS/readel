;;; readel.el --- Import Readeck annotations      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Elias Storms

;; Author: EFLS
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (request "0.3.3"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Basic retrieval system for Readeck annotations via API.
;;
;; Show or insert highlights (called annotations in Readeck parlance)
;; in Org-mode formatting.
;;
;; NOTE: The code below is very basic. There's no error handling when
;; the API request fails, or any sort of fallback. It's mostly written
;; for personal use, but feel free to expand on it.
;;
;; Readel provides two user-facing functions:
;; - `readel-annotations-buffer-render' to select a bookmark and display
;;   it's annotations in a separate buffer.
;; - `readel-annotations-insert-from-bm' to insert annotations at point.
;;
;; To use Readel, set the following parameters:
;; (setq readel-url "https://where.ever"  ;; Readeck URL
;;       readel-token "abc123"            ;; Readeck API token
;;       readel-bookmarks-label "label")  ;; Label to filter bookmarks


;;; Code:
(require 'request)

;;; Configuration parameters

(defcustom rd-url ""
  "Readeck host.
Enter the host URL without trailing slash."
  :type 'string
  :group 'readel)

(defcustom rd-token ""
  "Token for API access at Readeck host."
  :type 'string
  :group 'readel)

(defcustom rd-bookmarks-label ""
  "Label that acts as filter for shown bookmarks.
Set to nil to disable filtering and show all bookmarks."
  :type 'string
  :group 'readel)

(defcustom rd-buffer-name "*readel-annotations*"
  "Buffer name for Readel annotations buffer."
  :type 'string
  :group 'readel)


;;; Variables for storage

(defvar rd--bookmarks nil
  "Storage for Readeck bookmarks.")
(defvar rd--annotations nil
  "Storage for all Readeck annotations, grouped by bookmark ID.")
(defvar rd--bm-info nil
  "Storage for a single bookmarks metadata.")
(defvar rd--bm-annotations nil
  "Storage for a single bookmarks annotations.")


;;; Fetching data
;; The following functions store Readeck data via API.
;;
;; With `:sync t', request waits until the request is done. This
;; ensures the stored data is available after the function is called.

(defun rd--store-bookmarks ()
  "Via API, store all bookmarks in `rd--bookmarks'.
Only stores bookmarks with label `rd-bookmarks-label'."
  (request
    (format "%s/api/bookmarks" rd-url)
    :parser 'json-read
    :headers `(("Accept" . "application/json")
               ("Content-Type" . "application/json")
               ("Authorization" . ,(format "Bearer %s" rd-token)))
    :params `(("labels" . ,(or rd-bookmarks-label "")))
    :type "GET"
    :sync t
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                ;; Store the returned data in `rd--bookmarks', as an
                ;; association list grouped by title
                (setq rd--bookmarks
                      (seq-group-by
                       (lambda (x) (alist-get 'title x))
                       (seq-into data 'list)))))))

(defun rd--store-info-from-id (id)
  "Via API, store article info from bookmark `ID' in `rd--bm-info'."
  (request
    (format "%s/api/bookmarks" rd-url)
    :parser 'json-read
    :headers `(("Accept" . "application/json")
               ("Content-Type" . "application/json")
               ("Authorization" . ,(format "Bearer %s" rd-token)))
    :params `(("id" . ,id))
    :type "GET"
    :sync t
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (setq rd--bm-info
                      (car (seq-into data 'list)))))))

(defun rd--store-annotations-from-id (id)
  "Via API, store annotations from bookmark `ID' in `rd--bm-annotations'."
  (request
    (format "%s/api/bookmarks/%s/annotations" rd-url id)
    :parser 'json-read
    :headers `(("Accept" . "application/json")
               ("Content-Type" . "application/json")
               ("Authorization" . ,(format "Bearer %s" rd-token)))
    :type "GET"
    :sync t
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (setq rd--bm-annotations
                      (seq-into data 'list))))))


;; Helper functions to write data

(defun rd--buffer-write-front ()
  "Insert front matter info for stored bookmark."
  (let ((title (alist-get 'title rd--bm-info))
        (site (alist-get 'site_name rd--bm-info))
        (date (alist-get 'date rd--bm-info))
        (url (alist-get 'url rd--bm-info)))
  (insert "/" title "/\n"
          site ", " url "\n"
          "\n")))

(defun rd--buffer-write-annotations ()
  "Insert annotations stored for bookmark in Org-mode format."
  (dolist (q rd--bm-annotations)
    (insert "\n"
            "#+begin_quote" "\n"
            (alist-get 'text q)
            "\n"
            "#+end_quote" "\n" "\n")))
 

;;; Select Bookmark and return ID
;; TODO: Issue when titles are not unique?
(defun rd--select-bookmark-id ()
  "Interactively select a bookmark stored in `rd--bookmarks' and return its ID."
  (let* ((title (completing-read "Title: " rd--bookmarks))
         (bm (assoc title rd--bookmarks))
         (id (alist-get 'id (car (cdr bm)))))
    id))



;;; Main commands

;;;###autoload
(defun rd-annotations-insert-from-bm ()
  "Insert annotations from a selected article (tagged `rd-bookmarks-label')."
  (interactive)
  (rd--store-bookmarks)
  (let ((id (rd--select-bookmark-id)))
    (rd--store-info-from-id id)
    (rd--store-annotations-from-id id)
    (rd--buffer-write-front)
    (rd--buffer-write-annotations)))

;;;###autoload
(defun rd-annotations-buffer-render ()
  "Render annotations buffer."
  (interactive)
  (rd--store-bookmarks)
  (let ((id (rd--select-bookmark-id)))
    (rd--store-info-from-id id)
    (rd--store-annotations-from-id id)
    (with-current-buffer (get-buffer-create rd-buffer-name)
      (erase-buffer)
      (rd--buffer-write-front)
      (rd--buffer-write-annotations)
      (org-mode))
    (switch-to-buffer rd-buffer-name)))

(provide 'readel)
;;; readel.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("rd-" . "readel-"))
;; End:
