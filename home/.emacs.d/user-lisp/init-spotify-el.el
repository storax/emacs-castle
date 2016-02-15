;;; init-spotify-el --- Control spotify

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'url)
(require 'json)
(require 'helm)
(require 'init-utils)
(add-to-list 'load-path (expand-file-name "spotify.el" user-emacs-directory))
(require 'spotify)

;; Do not use values larger than 50 for better compatibility across endpoints
(setq spotify-api-search-limit 50)
(setq spotify-transport 'dbus)

(defvar storax/spotify-helm-current-playlist nil
  "Current playlist if viewing tracks of playlist.")

(defvar storax/spotify-helm-last-result nil
  "Last result of getting tracks if viewing tracks of playlist.")
(make-variable-buffer-local 'storax/spotify-helm-last-result)

(defvar storax/spotify-helm-current-tracks nil
  "Current tracks if viewing tracks of playlist.")
(make-variable-buffer-local 'storax/spotify-helm-current-tracks)

(defvar storax/spotify-connected nil
  "Have we connected?")

(cl-defun storax/helm-marked-candidates (&key with-wildcard)
  "Return marked candidates of current source if any.

When key WITH-WILDCARD is specified try to expand a wilcard if some."
  (with-current-buffer helm-buffer
    (let ((candidates
	   (cl-loop with current-src = (helm-get-current-source)
		    for (source . real) in (reverse helm-marked-candidates)
		    when (equal (assq 'name source) (assq 'name current-src))
		    append (helm--compute-marked real source with-wildcard)
		    into cands
		    finally return cands)))
      candidates)))

(defun storax/start-spotify ()
  "Start spotify."
  (interactive)
  (let ((process-connection-type nil))
    (async-shell-command "pgrep \"spotify\" > /dev/null || spotify &; exit 0"))
  (storax/spotify-connect)
  (kill-buffer "*Async Shell Command*"))

(defun storax/spotify-connect ()
  "Start a new Spotify session."
  (interactive)
  (require 'init-secrets)
  (spotify-connect)
  (global-spotify-remote-mode t)
  (setq storax/spotify-connected t))

(defun storax/spotify-disconnect ()
  "Start a new Spotify session."
  (interactive)
  (spotify-disconnect)
  (global-spotify-remote-mode -1)
  (setq storax/spotify-connected nil))

(defun storax/spotify-play-or-add-track (track)
  "Get the Spotify app to play the TRACK.

If marked tracks add them to a playlist."
  (let ((marked (storax/helm-marked-candidates))
	(selection (nth 0 (helm-marked-candidates))))
    (if marked
	(storax/spotify-add-tracks marked)
      (storax/spotify-play-track-hash selection))))

(defun storax/spotify-play-track-hash (track)
  "Get the Spotify app to play the TRACK."
  (spotify-play-track (gethash 'uri track)))

(defun storax/spotify-play-track-album (track)
  "Get the Spotify app to play the album of TRACK."
  (spotify-play-track (gethash 'href (gethash 'album track))))

(defun storax/spotify-play-playlist (playlist)
  "Get the Spotify app to play the PLAYLIST."
  (spotify-play-track (gethash 'uri playlist)))

(defun storax/spotify-helm-playlist-tracks (playlist)
  "Go to the tracks of PLAYLIST."
  (setq storax/spotify-helm-current-playlist playlist)
  (storax/spotify-helm--playlist-tracks))

(defun storax/spotify-helm-get-playlist-tracks ()
  "Go to the tracks of the current playlist."
  (let ((href (if storax/spotify-helm-last-result
		  (gethash 'next storax/spotify-helm-last-result)
		(gethash 'href (gethash 'tracks storax/spotify-helm-current-playlist)))))
    (if (and href
	     (setq storax/spotify-helm-last-result
		   (spotify-api-call "GET"
				     (car (cdr (split-string href spotify-api-endpoint))))))
	(setq storax/spotify-helm-current-tracks (append storax/spotify-helm-current-tracks
		(mapcar (lambda (track)
			  (cons (storax/spotify-format-track-hash (gethash 'track track))
				(gethash 'track track)))
			(gethash 'items storax/spotify-helm-last-result)))))
    storax/spotify-helm-current-tracks))

(defun storax/spotify-add-tracks (tracks)
  "Show playlists and add TRACKS to them."
  (let ((uristr (mapconcat (lambda (track) (gethash 'href track)) tracks ",")))
    (helm :sources '((name . "Spotify")
		     (multiline)
		     (candidates-process . storax/spotify-helm-get-my-playlists)
		     (action-transformer .
					 (lambda (actions playlist) `((,"Add tracks to playlist" .
					    (lambda (playlist) (spotify-api-call "POST"
					     (format "/users/%s/playlists/%s/tracks?uris=%s"
						     (spotify-current-user-id) (gethash 'id playlist) ',uristr))))))))
	:buffer "*Spotify: My playlists*"
	:prompt "Add tracks to playlists: ")))

(defun storax/spotify-track-search (search-term)
  "Search spotify for SEARCH-TERM, returning the results as a Lisp structure."
  (spotify-api-call "GET"
		    (format "/search?q=%s&type=track&limit=%d&market=from_token"
		     (url-hexify-string search-term) spotify-api-search-limit)))

(defun storax/spotify-playlist-search (search-term)
  "Search spotify for SEARCH-TERM, returning the results as a Lisp structure."
  (spotify-api-call "GET"
		    (format "/search?q=%s&type=playlist&limit=%d&market=from_token"
		    (url-hexify-string search-term) spotify-api-search-limit)))

(defun storax/spotify-my-playlists ()
  "Get my Spotify playlists, returning the results as a Lisp structure."
  (spotify-api-call "GET"
		    (format "/me/playlists?limit=%d" spotify-api-search-limit)))

(defun storax/spotify-format-track-hash (track)
  "Given a TRACK, return a formatted string suitable for display."
  (let ((track-name   (gethash 'name track))
	(track-length (gethash 'duration_ms track))
	(album-name   (gethash 'name (gethash 'album track)))
	(artist-names (mapcar (lambda (artist)
				(gethash 'name artist))
			      (gethash 'artists track)))
	(popularity (gethash 'popularity track)))
    (format "%s%s%s %2dm %2ds\n%s - %s"
	    (propertize track-name 'face '(:foreground "orange"))
	    (make-string (if (< (- fill-column (length track-name)) 1) 1
			   (- fill-column (length track-name))) 32)
	    (spotify-popularity-bar popularity)
	    (/ track-length 60000) (/ (mod track-length 60000) 1000)
	    (propertize (mapconcat 'identity artist-names "/") 'face '(:foreground "#FFDEAD"))
	    album-name)))

(defun storax/spotify-format-playlist (playlist)
  "Given a PLAYLIST, return a formatted string suitable for display."
  (let ((playlist-name (gethash 'name playlist))
	(tracks-count (gethash 'total (gethash 'tracks playlist)))
	(user (gethash 'id (gethash 'owner playlist))))
    (format "%s\nTracks: %4d | User: %s"
	    (propertize playlist-name 'face '(:foreground "orange"))
	    tracks-count
	    user)))

(defun storax/spotify-track-search-formatted (search-term)
  "Search tracks with SEARCH-TERM and format the result."
  (mapcar (lambda (track)
	    (cons (storax/spotify-format-track-hash track) track))
	  (gethash 'items (gethash 'tracks (storax/spotify-track-search search-term)))))

(defun storax/spotify-playlist-search-formatted (search-term)
  "Search playlists with SEARCH-TERM and format the result."
  (mapcar (lambda (playlist)
	    (cons (storax/spotify-format-playlist playlist) playlist))
	  (gethash 'items (gethash 'playlists (storax/spotify-playlist-search search-term)))))

(defun storax/spotify-my-playlists-formatted ()
  "Search playlists with SEARCH-TERM and format the result."
  (mapcar (lambda (playlist)
	    (cons (storax/spotify-format-playlist playlist) playlist))
	  (gethash 'items (storax/spotify-my-playlists))))

(defun storax/spotify-helm-search-tracks ()
  "Return a formatted list."
  (storax/spotify-track-search-formatted helm-pattern))

(defun storax/spotify-helm-search-playlists ()
  "Return a formatted list."
  (storax/spotify-playlist-search-formatted helm-pattern))

(defun storax/spotify-helm-get-my-playlists ()
  "Return a formatted list."
  (storax/spotify-my-playlists-formatted))

(defun storax/spotify-helm-actions-for-track-hash (actions track)
  "Return a list of helm ACTIONS available for this TRACK."
  `((,(format "Play Track - %s" (gethash 'name track)) . storax/spotify-play-or-add-track)
    (,(format "Play Album - %s" (gethash 'name (gethash 'album track))) . storax/spotify-play-track-album)
    ("Show Track Metadata" . pp)))

(defun storax/spotify-helm-actions-for-playlist (actions playlist)
  "Return a list of helm ACTIONS available for this PLAYLIST."
  `((,(format "Play Playlist - %s" (gethash 'name playlist)) . storax/spotify-play-playlist)
    (,(format "Look at Tracks of - %s" (gethash 'name playlist)) . storax/spotify-helm-playlist-tracks)
    ("Show Track Metadata" . pp)))

;;;###autoload
(defvar storax/spotify-helm-track-search-source
  '((name . "Spotify")
    (volatile)
    (delayed)
    (multiline)
    (requires-pattern . 2)
    (candidates-process . storax/spotify-helm-search-tracks)
    (action-transformer . storax/spotify-helm-actions-for-track-hash)))

;;;###autoload
(defun storax/spotify-helm-tracks()
  "Search Spotify tracks with helm."
  (interactive)
  (helm :sources '(storax/spotify-helm-track-search-source)
	:buffer "*Spotify: Search tracks*"
	:prompt "Tracks: "))

;;;###autoload
(defvar storax/spotify-helm-playlist-search-source
  '((name . "Spotify")
    (volatile)
    (delayed)
    (multiline)
    (requires-pattern . 2)
    (candidates-process . storax/spotify-helm-search-playlists)
    (action-transformer . storax/spotify-helm-actions-for-playlist)))

;;;###autoload
(defun storax/spotify-helm-playlists()
  "Search Spotify playlists with helm."
  (interactive)
  (helm :sources '(storax/spotify-helm-playlist-search-source)
	:buffer "*Spotify: Search playlists*"
	:prompt "Playlists: "))

;;;###autoload
(defvar storax/spotify-helm-my-playlists-source
  '((name . "Spotify")
    (multiline)
    (candidates-process . storax/spotify-helm-get-my-playlists)
    (action-transformer . storax/spotify-helm-actions-for-playlist)))

;;;###autoload
(defun storax/spotify-helm-my-playlists ()
  "Show my Spotify playlists with helm."
  (interactive)
  (helm :sources '(storax/spotify-helm-my-playlists-source)
	:buffer "*Spotify: My playlists*"
	:prompt "Playlists: "))

(defvar storax/spotify-helm-playlist-tracks-source
  '((name . "Spotify")
    (multiline)
    (candidates-process . storax/spotify-helm-get-playlist-tracks)
    (action-transformer . storax/spotify-helm-actions-for-track-hash)))

(defun storax/spotify-helm--playlist-tracks ()
  "Show my Spotify playlists with helm."
  (helm :sources '(storax/spotify-helm-playlist-tracks-source)
	:buffer (format "*Spotify: Tracks of playlists: %s *"
			(gethash 'name storax/spotify-helm-current-playlist))
	:promt "Tracks: "
	:candidate-number-limit nil))

(define-key spotify-remote-mode-map (kbd "M-p") nil)
(define-key spotify-remote-mode-map (kbd "M-p M-i") nil)
(define-key spotify-remote-mode-map (kbd "M-p M-s") nil)
(define-key spotify-remote-mode-map (kbd "M-p M-r") nil)
(define-key spotify-remote-mode-map (kbd "M-p M-p") nil)
(define-key spotify-remote-mode-map (kbd "M-p M-b") nil)
(define-key spotify-remote-mode-map (kbd "M-p M-f") nil)
(define-key spotify-remote-mode-map (kbd "M-s M-i") 'spotify-player-info)
(define-key spotify-remote-mode-map (kbd "M-s M-s") 'spotify-toggle-shuffle)
(define-key spotify-remote-mode-map (kbd "M-s M-r") 'spotify-toggle-repeat)
(define-key spotify-remote-mode-map (kbd "M-s M-p") 'spotify-toggle-play)
(define-key spotify-remote-mode-map (kbd "M-s M-b") 'spotify-previous-track)
(define-key spotify-remote-mode-map (kbd "M-s M-f") 'spotify-next-track)
(define-key spotify-remote-mode-map (kbd "M-s M-h M-t") 'storax/spotify-helm-tracks)
(define-key spotify-remote-mode-map (kbd "M-s M-h M-p") 'storax/spotify-helm-playlists)
(define-key spotify-remote-mode-map (kbd "M-s M-h M-m") 'storax/spotify-helm-my-playlists)

(defalias 'start-spotify 'storax/start-spotify)

(provide 'init-spotify-el)
;;; init-spotify-el ends here
