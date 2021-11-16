;;; smudge-podcast.el --- Smudge podcast search major mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2021 Isaac Leonard

;; SPDX-License-Identifier:  GPL-3.0-or-later

;;; Commentary:

;; This library implements UI and a major mode for searching and acting on Spotify podcasts.
;; Pretty much copied from the playlists file
;; TODO: Refactor

;;; Code:

(require 'smudge-api)
(require 'smudge-controller)
(require 'smudge-track)

(defvar smudge-user-id)
(defvar smudge-current-page)
(defvar smudge-browse-message)
(defvar smudge-selected-podcast)
(defvar smudge-query)

(defvar smudge-podcast-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "M-RET") 'smudge-podcast-select)
    (define-key map (kbd "l")     'smudge-podcast-load-more)
    (define-key map (kbd "g")     'smudge-podcast-reload)
    (define-key map (kbd "f")     'smudge-podcast-follow)
    (define-key map (kbd "u")     'smudge-podcast-unfollow)
    map)
  "Local keymap for `smudge-podcast-search-mode' buffers.")

(define-derived-mode smudge-podcast-search-mode tabulated-list-mode "Podcast-Search"
  "Major mode for displaying the podcasts returned by a Spotify search.")

(defun smudge-podcast-select ()
  "Plays the podcast under the cursor."
  (interactive)
  (let ((selected-podcast (tabulated-list-get-id)))
    (smudge-controller-play-track nil selected-podcast)))

(defun smudge-podcast-reload ()
  "Reloads the first page of results for the current podcast view."
  (interactive)
  (let ((page 1))
    (cond ((bound-and-true-p smudge-query)          (smudge-podcast-search-update smudge-query page))
          ((bound-and-true-p smudge-browse-message) (smudge-podcast-featured-podcasts-update page))
          (t                                         (smudge-podcast-user-podcasts-update (smudge-api-get-item-id smudge-user) page)))))

(defun smudge-podcast-load-more ()
  "Load the next page of results for the current podcast view."
  (interactive)
  (let ((next-page (1+ smudge-current-page)))
    (cond ((bound-and-true-p smudge-query)          (smudge-podcast-search-update smudge-query next-page))
          ((bound-and-true-p smudge-browse-message) (smudge-podcast-featured-podcasts-update next-page))
          (t                                         (smudge-podcast-user-podcasts-update (smudge-api-get-item-id smudge-user) next-page)))))

(defun smudge-podcast-follow ()
  "Add the current user as the follower of the podcast under the cursor."
  (interactive)
  (let* ((selected-podcast (tabulated-list-get-id))
         (name (smudge-api-get-item-name selected-podcast)))
    (when (y-or-n-p (format "Follow podcast '%s'? " name))
      (smudge-api-podcast-follow
       selected-podcast
       (lambda (_)
         (message "Followed podcast '%s'" name))))))

(defun smudge-podcast-unfollow ()
  "Remove the current user as the follower of the podcast under the cursor."
  (interactive)
  (let* ((selected-podcast (tabulated-list-get-id))
         (name (smudge-api-get-item-name selected-podcast)))
    (when (y-or-n-p (format "Unfollow podcast '%s'? " name))
      (smudge-api-podcast-unfollow
       selected-podcast
       (lambda (_)
         (message "Unfollowed podcast '%s'" name))))))

(defun smudge-podcast-search-update (query page)
  "Fetch the given PAGE of QUERY results using the search endpoint."
  (let ((buffer (current-buffer)))
    (smudge-api-search
     'podcast
     query
     page
     (lambda (podcasts)
       (if-let ((items (smudge-api-get-search-podcast-items podcasts)))
           (with-current-buffer buffer
             (setq-local smudge-current-page page)
             (setq-local smudge-query query)
             (pop-to-buffer buffer)
             (smudge-podcast-search-print items page)
             (message "Podcast view updated"))
         (message "No more podcasts"))))))

(defun smudge-podcast-user-podcasts-update (user-id page)
  "Fetch PAGE of results using the podcast endpoint for USER-ID."
  (let ((buffer (current-buffer)))
    (smudge-api-user-podcasts
     user-id
     page
     (lambda (podcasts)
       (if-let ((items (smudge-api-get-items podcasts)))
           (with-current-buffer buffer
             (setq-local smudge-user-id user-id)
             (setq-local smudge-current-page page)
             (pop-to-buffer buffer)
             (smudge-podcast-search-print items page)
             (message "Podcast view updated"))
         (message "No more podcasts"))))))

(defun smudge-podcast-featured-podcasts-update (page)
  "Fetch PAGE of results using of Spotify's featured podcasts."
  (let ((buffer (current-buffer)))
    (smudge-api-featured-podcasts
     page
     (lambda (json)
       (if-let ((items (smudge-api-get-search-podcast-items json))
                (msg (smudge-api-get-message json)))
           (with-current-buffer buffer
             (setq-local smudge-current-page page)
             (setq-local smudge-browse-message msg)
             (pop-to-buffer buffer)
             (smudge-podcast-search-print items page)
             (message "Podcast view updated"))
         (message "No more podcasts"))))))

(defun smudge-podcast-tracks ()
  "Displays the tracks that belongs to the podcast under the cursor."
  (interactive)
  (let* ((selected-podcast (tabulated-list-get-id))
         (name (smudge-api-get-item-name selected-podcast))
         (buffer (get-buffer-create (format "*Podcast Tracks: %s*" name))))
    (with-current-buffer buffer
      (smudge-track-search-mode)
      (setq-local smudge-selected-podcast selected-podcast)
      (smudge-track-podcast-tracks-update 1))))

(defun smudge-podcast-set-list-format ()
  "Configures the column data for the typical podcast view."
  (setq tabulated-list-format
        (vector `("Podcast Name" ,(- (window-width) 45) t)
                '("Owner Id" 30 t)
                '("# Tracks" 8 (lambda (row-1 row-2)
                                 (< (smudge-api-get-podcast-track-count (car row-1))
                                    (smudge-api-get-podcast-track-count (car row-2)))) :right-align t))))

(defun smudge-podcast-search-print (podcasts page)
  "Append PODCASTS to PAGE of the current podcast view."
  (let (entries)
    (dolist (podcast podcasts)
      (let ((user-id (smudge-api-get-podcast-owner-id podcast))
            (podcast-name (smudge-api-get-item-name podcast)))
        (push (list podcast
                    (vector (cons podcast-name
                                  (list 'face 'link
                                        'follow-link t
                                        'action `(lambda (_) (smudge-podcast-tracks))
                                        'help-echo (format "Show %s's tracks" podcast-name)))
                            (cons user-id
                                  (list 'face 'link
                                        'follow-link t
                                        'action `(lambda (_) (smudge-user-podcasts ,user-id))
                                        'help-echo (format "Show %s's public podcasts" user-id)))
                            (number-to-string (smudge-api-get-podcast-track-count podcast))))
              entries)))
    (when (eq 1 page) (setq-local tabulated-list-entries nil))
    (smudge-podcast-set-list-format)
    (setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(provide 'smudge-podcast)
;;; smudge-podcast.el ends here
