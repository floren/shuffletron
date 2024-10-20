(in-package :shuffletron)

(defvar *library* nil)
(defvar *filtered-library* nil "The library, excluding songs tagged 'ignore'")
(defvar *local-path->song* (make-hash-table :test 'equal))
(defvar *cover-art* (make-hash-table :test 'equalp))
(define-symbol-macro *library-base* (pref "library-base"))

(defstruct song full-path local-path cover-path tags smashed properties matchprops id3 id3-p)

(defun init-library ()
  (setf *library* (make-array 0 :fill-pointer 0 :adjustable t)))

(defun match-extension (filename extension)
  (not (mismatch filename extension :test #'char-equal :start1 (- (length filename) (length extension)))))

(defun music-file-type (filename)
  (or (and (match-extension filename ".mp3") :mp3)
      (and (match-extension filename ".ogg") :ogg)
      (and (match-extension filename ".flac") :flac)))

(defvar *library-progress* 0)

(defun smash-string (string)
  (substitute #\Space #\_ (string-downcase string)))

(defun carriage-return () (format t "~C" (code-char 13)))

(defun add-song-file (full-filename relative-filename)
  (let* ((cover-path (uiop:file-exists-p (merge-pathnames #P"cover.jpg" (directory-namestring full-filename))))
	(song (make-song :full-path full-filename
                         :local-path relative-filename
			 :cover-path cover-path
                         :smashed (smash-string relative-filename)
                         :tags nil)))
    (vector-push-extend song *library*)
    (setf (gethash (song-local-path song) *local-path->song*) song)))

(defun library-scan (path)
  (let ((*library-progress* 0))
    (clrhash *local-path->song*)
    (when (probe-file path)
      (walk path
            (lambda (filename)
              (when (music-file-type filename)
                (incf *library-progress*)
                (when (zerop (mod *library-progress* 10))
                  (carriage-return)
                  (format t "Scanning. ~:D files.." *library-progress*)
                  (force-output))
                (add-song-file filename (relative-to path filename)))))
      t)))

(defun get-artist-covers (artist)
  (let* ((artist-covers (gethash artist *cover-art*)))
    (if artist-covers
	artist-covers
	(make-hash-table :test 'equalp))))

(defun get-artist-album-cover (artist album)
  (let* ((artist-covers (get-artist-covers artist))
	 (cover-path (gethash album artist-covers)))
    cover-path))

(defun build-covers-lookup ()
  "Run after the library is built, compiles a lookup table mapping artist & album to cover image path"
  (clrhash *cover-art*)
  (loop for song across *library* do
    (let* ((id3 (song-id3 song))
	   (artist (getf id3 :artist))
	   (album (getf id3 :album))
	   (artist-covers (get-artist-covers artist))
	   (album-cover (get-artist-album-cover artist album)))
      ;; If the album cover isn't defined yet, add it to the artist list
      (if (not album-cover)
	  (progn
	    (setf (gethash album artist-covers) (song-cover-path song))
	    (setf (gethash artist *cover-art*) artist-covers))))))

(defun songs-needing-id3-scan () (count-if-not #'song-id3-p *library*))

(defun save-metadata-cache ()
  (setf (pref "id3-cache")
        (map 'vector (lambda (song) (list (song-local-path song)
                                          (song-id3-p song)
                                          (song-id3 song)))
             *library*))
  (values))

(defun load-metadata-cache ()
  (loop for (name id3-p id3) across (pref "id3-cache" #())
        as song = (gethash name *local-path->song*)
        when (and song id3-p)
        do (setf (song-id3-p song) t
                 (song-id3 song) id3)))

(defun get-song-metadata (absolute-path)
  (progn ;ignore-errors                        ; I'm a bad person.
   (case (music-file-type absolute-path)
     (:mp3  (mpg123:get-tags-from-file absolute-path :no-utf8 nil :character-encoding :utf-8))
     ;; FIXME: Audit OGG/FLAC paths for unicode insanity.
     (:ogg  (vorbisfile:get-vorbis-tags-from-file absolute-path :character-encoding :utf-8))
     (:flac (flac:get-flac-tags-from-file absolute-path :character-encoding :utf-8)))))

(defun scan-file-metadata (&key verbose adjective)
  (format t "~&Scanning file metadata (~:D files).~%" (songs-needing-id3-scan))
  (when verbose (fresh-line))
  (loop with pending = (and verbose (songs-needing-id3-scan))
        with n = 1
        for song across *library*
        unless (song-id3-p song) do
        (when verbose
          (carriage-return)
          (format t "Reading ~Atags: ~:D of ~:D" (or adjective "") n pending)
          (force-output))
        (setf (song-id3 song) (get-song-metadata (song-full-path song))
              (song-matchprops song) nil
              (song-id3-p song) t)
        (incf n)
        finally
        (when (and pending (not (zerop pending))) (terpri)))
  (build-covers-lookup) ; if metadata changed, rebuild the covers lookup
  (save-metadata-cache))

(defun compute-filtered-library ()
  (setf *filtered-library* (remove-if (lambda (song) (find "ignore" (song-tags song) :test #'string=)) *library*)))
