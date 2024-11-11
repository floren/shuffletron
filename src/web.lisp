(in-package :shuffletron)

(defvar *clack-app* nil)
(defparameter *application-root*   (asdf:system-source-directory :shuffletron))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(djula:add-template-directory  (merge-pathnames #P"templates/" *application-root*))
(defparameter *template-registry* (make-hash-table :test 'equal))

(defvar *app* (make-instance 'ningle:<app>))

;; copied from caveman
(defun render (template-path &optional env)
  (let ((djula:*template-package* :shuffletron) (template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
           template nil
           env)))

(defun get-player-state ()
  (with-stream-control ()
    (let* ((paused (when *current-stream* (streamer-paused-p *current-stream* *mixer*)))
	   (remaining (if *current-stream*
			  (/ (- (streamer-length *current-stream* *mixer*) (streamer-position *current-stream* *mixer*)) (streamer-sample-rate *current-stream*))
			  0))
	   (songlength (if *current-stream*
			   (/ (streamer-length *current-stream* *mixer*) (streamer-sample-rate *current-stream*))
			   0)))
      (list :looping *loop-mode*
	    :paused paused
	    :remaining (floor remaining)
	    :remaining-min (floor (/ remaining 60))
	    :remaining-sec (floor (mod remaining 60))
	    :songlength (floor songlength)
	    :songlength-min (floor (/ songlength 60))
	    :songlength-sec (floor (mod songlength 60))))))

(defun get-param (name params)
  (cdr (assoc name params :test #'string=)))

(defun start-web ()
  ;; Define user interface endpoints
  (setf (ningle:route *app* "/")
	(lambda (params)
	  (with-playqueue ()
	    (render #P"index.html" (list
				    :state (get-player-state)
				    :current (get-current-song)
				    :queue *playqueue*)))))

  (setf (ningle:route *app* "/cover.jpg")
	(lambda (params)
	  (let* ((artist (get-param "artist" params))
		 (album (get-param "album" params)))
	    ;; If they specify an artist, return the first album cover for that artist
	    ;; If they specify artist plus album, return appropriate album cover
	    ;; If the specify neither, get the current cover
	    (if artist
		;; Artist specified, check if they also gave an album name
		(if album
		    ;; Album was specified, look it up
		    (let ((cover (get-artist-album-cover artist album)))
		      (if cover
			  (list 200 '(:cache-control "max-age=604800") (parse-namestring cover))
			  (list 404 '() nil))
		      )		    ;; No album specified, just try to return *a* cover for the artist
		    (let ((artist-covers (get-artist-covers artist)))
		      (unless artist-covers
			(list 404 '() nil))
		      (with-hash-table-iterator (next artist-covers)
			(loop
			  (multiple-value-bind (more? key value) (next)
			    (unless more? (return (list 404 '() nil)))
			    (return (list  200 '(:cache-control "max-age=31536000, immutable") (parse-namestring value))))))))
		;; No artist specified, just return current cover
		(let* ((song (get-current-song)))
		  (when song
		    (list 200 '(:cache-control "no-cache") (song-cover-path song))))))))

  (setf (ningle:route *app* "/artists")
	(lambda (params)
	  (let* ((artists (get-artist-list)))
	    (render #P"artists.html" (list
				      :artists artists)))))
  
  (setf (ningle:route *app* "/artists/:artist")
	(lambda (params)
	  (let* ((artist (get-param "ARTIST" params))
		 (songs (get-songs-by-artist artist)))
	    (render #P"artist.html" (list
				     :artist artist
				     :results songs)))))
  
  (setf (ningle:route *app* "/playlist/shuffle")
	(lambda (params)
	  (let* ((headers (lack/request:request-headers *request*))
		 (referer (gethash "referer" headers)))
	    (with-playqueue ()
	      (setf *playqueue* (alexandria:shuffle *playqueue*))
	      (list 302 (list :location referer '()))))))

  (setf (ningle:route *app* "/playlist/toggle_loop")
	(lambda (params)
	  (let* ((headers (lack/request:request-headers *request*))
		 (referer (gethash "referer" headers)))
	    (setf *loop-mode* (not *loop-mode*))
	    (list 302 (list :location referer '())))))
  
  (setf (ningle:route *app* "/next")
	(lambda (params)
	  (let* ((headers (lack/request:request-headers *request*))
		 (referer (gethash "referer" headers)))
	    (play-next-song)
	    (list 302 (list :location referer) '()))))

  (setf (ningle:route *app* "/pause")
	(lambda (params)
	  (let* ((headers (lack/request:request-headers *request*))
		 (referer (gethash "referer" headers)))
	    (toggle-pause)
	    (list 302 (list :location referer) '()))))

  (setf (ningle:route *app* "/playlist/clear" :method :GET)
	(lambda (params)
	  (with-playqueue ()
	    (setf *playqueue* nil)
	    (list 302 '(:location "/") '()))))

  (setf (ningle:route *app* "/search" :method :GET)
	(lambda (params)
	  (let* ((query (cdr (assoc "query" params :test #'string=)))
		 (results (do-query query nil)))
	    (reset-query)
	    (render #P"search.html" (list
				     :query query
				     :results results)))))

  (setf (ningle:route *app* "/prepend")
	;; If "query" is specified, search for that.
	;; Otherwise, filter by artist and/or album
	(lambda (params)
	  (let* ((query (get-param "query" params))
		 (artist (get-param "artist" params))
		 (album (get-param "album" params))
		 (title (get-param "title" params)))
	    (if query
		(let ((results (do-query query nil)))
		  (reset-query)
		  (play-songs results)
		  (list 302 '(:location "/") '()))
		(let ((results (do-advanced-query :artist artist :album album :title title)))
		  (play-songs results)
		  (list 302 '(:location "/") '()))))))

  (setf (ningle:route *app* "/append")
	;; If "query" is specified, search for that.
	;; Otherwise, filter by artist and/or album
	(lambda (params)
	  (let* ((query (get-param "query" params))
		 (artist (get-param "artist" params))
		 (album (get-param "album" params))
		 (title (get-param "title" params)))
	    (if query
		(let ((results (do-query query nil)))
		  (reset-query)
		  (add-songs results)
		  (list 302 '(:location "/") '()))
		(let ((results (do-advanced-query :artist artist :album album :title title)))
		  (add-songs results)
		  (list 302 '(:location "/") '()))))))

  ;; Define the JSON API
  (setf (ningle:route *app* "/api/current")
	#'(lambda (params)
	    (encode-json (get-current-song))))
  (setf (ningle:route *app* "/api/stop")
	#'(lambda (params)
	    (progn
	      (stop-command)
	      (encode-json (get-current-song)))))
  (setf (ningle:route *app* "/api/play")
	#'(lambda (params)
	    (progn
	      (play-command)
	      (encode-json (get-current-song)))))
  (setf (ningle:route *app* "/api/pause")
	#'(lambda (params)
	    (progn
	      (toggle-pause)
	      (encode-json (get-current-song)))))
  (setf (ningle:route *app* "/api/next")
	#'(lambda (params)
	    (progn
	      (play-next-song)
	      (encode-json (get-current-song)))))
  (setf (ningle:route *app* "/api/queue" :method :GET)
	#'(lambda (params)
	    (with-playqueue ()  (encode-json *playqueue*))))
  (setf (ningle:route *app* "/api/queue" :method :DELETE)
	#'(lambda (params)
	    (with-playqueue ()
	      (setf *playqueue* nil))))
  (setf (ningle:route *app* "/api/search" :method :GET)
	#'(lambda (params)
	    (let* ((query (cdr (assoc "query" params :test #'string=)))
		   (results (do-query query nil)))
	      (reset-query)
	      (encode-json results))))
  (setf (ningle:route *app* "/api/prepend" :method :GET)
	#'(lambda (params)
	    (let* ((query (cdr (assoc "query" params :test #'string=)))
		   (results (do-query query nil)))
	      (reset-query)
	      (play-songs results)
	      (with-playqueue ()  (encode-json *playqueue*)))))
  (setf (ningle:route *app* "/api/append" :method :GET)
	#'(lambda (params)
	    (let* ((query (cdr (assoc "query" params :test #'string=)))
		   (results (do-query query nil)))
	      (reset-query)
	      (add-songs results)
	      (with-playqueue () (encode-json *playqueue*)))))

  ;; Handle static files
  (setf *clack-app* (clack:clackup (lack:builder
				    :session
				    (:static :path "/static/"
					     :root *static-directory*)
				    (lambda (app)
				      (lambda (env)
					(funcall app env)))
				    *app*) :address "0.0.0.0" :debug nil)))

