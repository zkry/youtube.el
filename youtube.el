;;; youtube.el --- Manage downloaded YouTube videos  -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ((f "0.20.0") (request "0.3.2"))
;; Homepage: https://github.com/zkry/youtube
;; Keywords: tool


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This package provides a simple interface for searching for YouTube
;; videos, via the `youtube-search' command, and viewing the saved
;; videos via the `youtube-list' command.  This package expects you to
;; define the following variables:
;;
;; `youtube-api-key' contains an API key which you can use to access
;; the YouTube Data API v3.
;;
;; `youtube-storage-dir' is set to a directory where you want all the
;; videos to be saved to.

;;; Code:

(require 'f)
(require 'request)

(defvar youtube-api-key nil
  "The API key used to access Google YouTube Data API.")

(defcustom youtube-storage-dir "~/.youtube/"
  "Defines directory where YouTube media is stored."
  :type 'directory
  :package-version '(youtube . "0.1.0"))

(defvar youtube-saved-files nil
  "Alist containing the contents of the saved YouTube directory.")

(defun youtube--saved-file-name ()
  "Return the filename of the persisted saved videos cache."
  (concat (file-name-as-directory youtube-storage-dir) "saved-videos.eld"))

(defun youtube--load-saved-files ()
  "Load the video cache from the saved videos file in the youtube storage directory."
  (let ((filename (youtube--saved-file-name)))
    (if (not (f-exists? filename))
        (setq youtube-saved-files nil)
      (let ((data (read (f-read filename))))
        (setq youtube-saved-files data)))))

(defun youtube--save-file-list ()
  "Save the video cache to the youtube storage directory."
  (unless (f-exists? youtube-storage-dir)
    (f-mkdir youtube-storage-dir))
  (let ((filename (youtube--saved-file-name)))
    (with-temp-buffer
      (prin1 youtube-saved-files (current-buffer))
      (write-file filename))))

(defun youtube--save-file (title video-id publish-date channel-title)
  "Save a video with TITLE, VIDEO-ID, PUBLISH-DATE, and CHANNEL-TITLE to cache."
  (unless youtube-saved-files
    (youtube--load-saved-files))
  (unless (seq-some (lambda (item) (equal video-id (alist-get 'video-id item)))
                    youtube-saved-files)
    (setq youtube-saved-files
          (cons `((title . ,title)
                  (video-id . ,video-id)
                  (publish-date . ,publish-date)
                  (channel-title . ,channel-title))
                youtube-saved-files))
    (youtube--save-file-list)))

(defface youtube-search-date-face
  '((((class color) (background light)) (:foreground "#aaa"))
    (((class color) (background dark))  (:foreground "#77a")))
  "Face used in search for dates."
  :group 'youtube)

(defface youtube-search-channel-face
  '((((class color) (background light)) (:foreground "#148"))
    (((class color) (background dark))  (:foreground "#58b")))
  "Face used in search for channels."
  :group 'youtube)

(defface youtube-search-next-page-button-face
  '((((class color) (background light)) (:foreground "#aa2"))
    (((class color) (background dark))  (:foreground "#ffb")))
  "Face used in search for channels."
  :group 'youtube)

(defvar youtube-search-term nil
  "The search term that was used in previous search.")

(defun youtube--search-url (term)
  "Generate the URL used to perform a YouTube search for TERM."
  (format "https://youtube.googleapis.com/youtube/v3/search?part=snippet&maxResults=50&q=%s&key=%s"
          (url-hexify-string term) youtube-api-key))

(defun youtube--video-info-url (id)
  "Generate the URL used to perform a YouTube video fetch for video with ID."
  (format "https://youtube.googleapis.com/youtube/v3/videos?id=%s&part=snippet,contentDetails,statistics,status&key=%s"id youtube-api-key))

(defun youtube--render-search-buffer (ret-data next-page-token &optional skip-erase-buffer)
  "Renders the data to the search results buffer with data RET-DATA.

A button is added to the end to fetch next page indicated by
NEXT-PAGE-TOKEN.  If SKIP-ERASE-BUFFER is non-nil then, append
the new items to the page rather than erase the buffer."
  (with-current-buffer "*youtube-search*"
    (let ((inhibit-read-only t))
      (unless skip-erase-buffer
        (erase-buffer))
      (dolist (item ret-data)
        (let* ((title (alist-get 'title item))
               (description (alist-get 'description item))
               (video-id (alist-get 'video-id item))
               (publish-date (substring (alist-get 'publish-time item) 0 10))
               (channel-title (alist-get 'channel-title item))
               (line (format "  %-10.10s %-75.75s %s"
                             (propertize publish-date 'face 'youtube-search-date-face)
                             (propertize title 'face 'bold)
                             (propertize channel-title 'face 'youtube-search-channel-face))))
          (insert (propertize line
                              'youtube-title title
                              'youtube-description description
                              'youtube-video-id video-id
                              'youtube-publish-date publish-date
                              'youtube-channel-title channel-title)
                  "\n")))
      (insert (propertize "next page"
                          'youtube-next-page-token next-page-token
                          'face 'youtube-search-next-page-button-face)))))

(defun youtube--extract-item-data (item)
  "Given an API call search ITEM, return an alist of desired data."
  (let* ((video-id (alist-get 'videoId (alist-get 'id item)))
         (snippet (alist-get 'snippet item))
         (title (alist-get 'title snippet))
         (channel-title (alist-get 'channelTitle snippet))
         (publish-time (alist-get 'publishedAt snippet))
         (description (alist-get 'description snippet))
         (thumbnail-url (alist-get 'url (alist-get 'default (alist-get 'thumbnails snippet)))))
    `((video-id . ,video-id)
      (title . ,title) ;; TODO: replace &quot and &amp
      (description . ,description)
      (thumbnail-url . ,thumbnail-url)
      (channel-title . ,channel-title)
      (publish-time . ,publish-time))))

(defun youtube--search (term)
  "Perform a GET request to search for TERM."
  (setq-local youtube-search-term term)
  (let ((url (youtube--search-url term)))
    (request url
      :type "GET"
      :parser 'json-read
      :success (cl-function (lambda (&key data &allow-other-keys)
                               ;; items id videoId
                               ;;       snippet title
                               ;;               description
                               ;;               thumbnails default url
                               (let ((ret-data))
                                 (let-alist data
                                   (let ((next-page-token (alist-get 'nextPageToken data)))
                                     (seq-do
                                      (lambda (item)
                                        (setq ret-data (cons (youtube--extract-item-data item) ret-data)))
                                      .items)
                                     (youtube--render-search-buffer ret-data next-page-token)))))))))

(defun youtube-search--download-audio-item-at-point ()
  "Same as youtube-search--download-item-at-point but downloads the audio."
  (interactive)
  (youtube-search--download-item-at-point t))

(defun youtube-search--append-page-data (data next-page-token)
  "Given the next page search DATA add the data to the list of results.

NEXT-PAGE-TOKEN should be a token pointing to the next page."
  (with-current-buffer "*youtube-search*"
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (crux-kill-whole-line)
      (insert "---\n")
      (youtube--render-search-buffer data next-page-token t))))

(defun youtube-search--fetch-video-data (video-id)
  (request (youtube--video-info-url video-id)
    :type "GET"
    :parser 'json-read
    :success (cl-function (lambda (&key data &allow-other-keys)
                            (let* ((item (aref (alist-get 'items data) 0))
                                   (snippet (alist-get 'snippet item))
                                   (tags (alist-get 'tags snippet))
                                   (description (alist-get 'description snippet))

                                   (statistics (alist-get 'statistics item))
                                   (view-count (alist-get 'viewCount statistics))
                                   (like-count (alist-get 'likeCount statistics)))
                              `((tags . ,tags)
                                (description . ,description)
                                (view-count . ,view-count)
                                (like-count . ,like-count))
                              ;; TODO hack because it's not used anywhere elsee
                              (message "views: %s, likes: %s\n\n%s"
                                       view-count
                                       like-count
                                       description))))))

(defun youtube-search--fetch-page (next-page-token)
  (let ((url (concat (youtube--search-url youtube-search-term) "&pageToken=" next-page-token)))
    (request url
      :type "GET"
      :parser 'json-read
      :success (cl-function (lambda (&key data &allow-other-keys)
                              ;; items id videoId
                              ;;       snippet title
                              ;;               description
                              ;;               thumbnails default url
                              (let ((ret-data))
                                (let-alist data
                                  (let ((next-page-token (alist-get 'nextPageToken data)))
                                    (seq-do
                                     (lambda (item)
                                       (setq ret-data (cons (youtube--extract-item-data item) ret-data)))
                                     .items)
                                    (youtube-search--append-page-data ret-data next-page-token)))))))))

(defun youtube-search--download-item-at-point-or-next-page (&optional audio-only)
  "Download the video which point is currently on.

If AUDIO-ONLY is non-nil, download the file in mp3 format."
  (interactive)
  (if-let ((next-page-token (get-text-property (point) 'youtube-next-page-token)))
      (youtube-search--fetch-page next-page-token)
    (let* ((title (get-text-property (point) 'youtube-title))
           (video-id (get-text-property (point) 'youtube-video-id))
           (publish-date (get-text-property (point) 'youtube-publish-date))
           (channel-title (get-text-property (point) 'youtube-channel-title))
           (url (format "https://www.youtube.com/watch?v=%s" video-id))
           (filename-base (format "%s-%s" video-id title))
           (filename (format "%s.%%(ext)s" filename-base))
           (cmd (if audio-only
                    (list "youtube-dl"
                          "--output" filename
                          "-x"
                          "--audio-format" "mp3"
                          url)
                  (list "youtube-dl"
                        "-f" "bestvideo[ext=mp4]+bestaudio[ext=m4a]/mp4"
                        "--output" filename
                        url))))
      (unless video-id
        (error "No video found at point"))
      (let ((default-directory (file-name-as-directory youtube-storage-dir)))
        (make-process :name "youtube-dl"
                      :buffer "*youtube-dl*"
                      :command cmd
                      :sentinel `(lambda (process event)
                                   (message "got event: %S" event)
                                   (when (equal event  "finished\n")
                                     (with-current-buffer "*youtube-dl*"
                                       (goto-char (point-max))
                                       (next-line -1)
                                       (let ((filename (string-trim-right (substring-no-properties (thing-at-point 'line)))))
                                         (youtube--save-file ,title ,video-id ,publish-date ,channel-title)
                                         (message "Video %S finished downloading" ,title)))))))
      (message "Downloading file..."))))

(defun youtube-search--file-info-at-point ()
  (interactive)
  (let ((video-id (get-text-property (point) 'youtube-video-id)))
    (unless video-id
      (error "No file found at point"))
    (youtube-search--fetch-video-data video-id)))

(defun youtube-search--help ()
  "Display the key bindings for youtube-search mode."
  (interactive)
  (message "i - video info\nq - quit window\no - open file at point (if downloaded)\ns - perform a new search\n<return> - download item"))

(defvar youtube-search-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "i" #'youtube-search--file-info-at-point)
      (define-key map "?" #'youtube-search--help)
      (define-key map "q" #'quit-window)
      (define-key map "n" #'next-line)
      (define-key map "o" #'youtube-list--open-file-at-point)
      (define-key map "s" #'youtube-search)
      (define-key map "p" #'previous-line)
      (define-key map "a" #'youtube-search--download-audio-item-at-point)
      (define-key map (kbd "<return>") #'youtube-search--download-item-at-point-or-next-page)))
  "Keymap for youtube-search-mode.")

(defun youtube-search-mode (search-term)
  "Major mode for viewing list of searched YouTube videos for SEARCH-TERM.

\\{youtube-seach-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map youtube-search-mode-map)
  (setq major-mode 'youtube-search-mode
        mode-name "youtube-search"
        truncate-lines t
        buffer-read-only t
        header-line-format (format "Search results for: %s" search-term))
  (buffer-disable-undo)
  (hl-line-mode))

(defun youtube-search (search-term)
  "Search YouTube for videos to later download for given SEARCH-TERM."
  (interactive "sSearch:")
  (switch-to-buffer (get-buffer-create "*youtube-search*"))
  (setq header-line-format (format "Search results for: %s" search-term))
  (unless (eq major-mode 'youtube-search-mode)
    (youtube-search-mode search-term))
  (youtube--search search-term))


;;; youtube-list

(defun youtube--find-file-by-id (video-id)
  (let ((default-directory (file-name-as-directory youtube-storage-dir))
        (files (directory-files youtube-storage-dir))
        (ret-file))
    (progn
      (while files
        (let ((file (car files)))
          (when (string-prefix-p video-id file)
            (setq ret-file file)
            (setq files nil))
          (setq files (cdr files))))
      ret-file)))

(defun youtube--sync-saved-files-cache ()
  (setq youtube-saved-files
        (seq-filter (lambda (file)
                      (youtube--find-file-by-id (alist-get 'video-id file)))
                    youtube-saved-files))
  (youtube--save-file-list))

(defun youtube-open-file (file)
  "Open FILE."
  (call-process "open" nil nil nil file))

(defun youtube-list--open-file-by-id (video-id)
  "Open the video file with the given VIDEO-ID."
  (let ((default-directory (file-name-as-directory youtube-storage-dir))
        (files (directory-files youtube-storage-dir))
        (found))
    (while files
      (let ((file (car files)))
        (when (string-prefix-p video-id file)
          (youtube-open-file file)
          (setq files nil)
          (setq found t))
        (setq files (cdr files))))
    (unless found
      (error "File not found on disk"))))

(defun youtube-list--delete-file-by-id (video-id)
  "Open the video file with the given VIDEO-ID."
  (let ((default-directory (file-name-as-directory youtube-storage-dir))
        (files (directory-files youtube-storage-dir)))
    (while files
      (let ((file (car files)))
        (when (string-prefix-p video-id file)
          (call-process "rm" nil nil nil file)
          (setq youtube-saved-files (seq-filter (lambda (item)
                                                  (not (equal (alist-get 'video-id item) video-id)))
                                                youtube-saved-files))
          (youtube--save-file-list)
          (message "deleted file %S" file)
          (setq files nil))
        (setq files (cdr files))))))

(defun youtube-list--open-file-at-point ()
  ""
  (interactive)
  (let ((video-id (get-text-property (point) 'youtube-video-id)))
    (unless video-id
      (error "No video found under point"))
    (youtube-list--open-file-by-id video-id)))

(defun youtube-list--delete-file-at-point ()
  (interactive)
  (let ((video-id (get-text-property (point) 'youtube-video-id)))
    (unless video-id
      (error "No video found under point"))
    (youtube-list--delete-file-by-id video-id)
    (youtube-list--display)))

(defun yotube-list--help ()
  "Prints the main keybindings of youtube-list mode."
  (message "d - delete file\ns - perform a search\nq - quit window\n<return> - open file"))

(defvar youtube-list-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "d" #'youtube-list--delete-file-at-point)
      (define-key map "?" #'youtube-list--help)
      (define-key map "q" #'quit-window)
      (define-key map "n" #'next-line)
      (define-key map "p" #'previous-line)
      (define-key map "s" #'youtube-search)
      (define-key map "g" #'youtube-list--display)
      (define-key map (kbd "<return>") #'youtube-list--open-file-at-point)))
  "Keymap for youtube-search-mode.")

(defun youtube-list--display ()
  "Display the saved list of videos to the buffer."
  (interactive)
  (youtube--load-saved-files)
  (youtube--sync-saved-files-cache)
  (let ((inhibit-read-only t))
    (with-current-buffer (get-buffer-create "*youtube-list*")
      (erase-buffer)
      (seq-do (lambda (file)
                (message "%S" file)
                (let-alist file
                  (let ((line (format "  %10.10s %-75.75s %s\n"
                                      (propertize .publish-date 'face 'youtube-search-date-face)
                                      (propertize .title 'face 'bold)
                                      (propertize .channel-title 'face 'youtube-search-channel-face))))
                    (insert (propertize line 'youtube-video-id .video-id)))))
              youtube-saved-files))))

(defun youtube-list-mode ()
  "Major mode for viewing saved YouTube videos."
  (interactive)
  (kill-all-local-variables)
  (use-local-map youtube-list-mode-map)
  (setq major-mode 'youtube-list-mode
        mode-name "youtube-list"
        truncate-lines t
        buffer-read-only t)
  (buffer-disable-undo)
  (hl-line-mode)
  (youtube-list--display))

(defun youtube-list ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*youtube-list*"))
  (unless (eq major-mode 'youtube-search-mode)
    (youtube-list-mode)))

(provide 'youtube)

;;; youtube.el ends here
