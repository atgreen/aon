(in-package :aon)

(defun read-glyph (stream)
  "Read a character bitmap from a stream, followed by two blank lines. Each character
is 5 lines high."
  (let ((bitmap (reverse (loop for i from 0 to 4
                               collect (read-line stream)))))
    (read-line stream)
    bitmap))

(defun glyph-width (glyph)
  (loop for line in glyph
        maximize (length line)))

(defun read-font-file ()
  (let ((glyphs (make-hash-table :test 'equal)))
    (with-open-file (stream "font.txt" :direction :input)
      (loop for char across "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZhm :-"
            do (setf (gethash char glyphs) (read-glyph stream))))
    glyphs))

(defparameter *day-names*
  '("MON" "TUE" "WED" "THU" "FRI" "SAT" "SUN"))

(defparameter *month-names*
  '("" "JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"))

(defparameter *glyphs* (read-font-file))

(defun next-call ()
  (let ((next-call-time (drakma:http-request (get-secret :gb-api-endpoint)
                                             :method :get
                                             :basic-authorization (list (get-secret :gb-api-username) (get-secret :gb-api-password)))))
    next-call-time))

(defun seconds-util-next-call ()
  (- (date-time-parser:parse-date-time (next-call))
     (get-universal-time)))

(defun string-width (string)
  (loop for char across string
        summing (+ 1 (glyph-width (gethash char *glyphs*)))))

(defun update-pixoo ()
  (when (equal *network* "HOME")
    (generate-asset-png)
    (let ((pic-data
            (let* ((surface (cairo:create-image-surface :rgb24 64 64))
                   (ctx (cairo:create-context surface)))
                (unwind-protect
                     (cairo:with-context (ctx)
                       (labels ((draw-glyph (char x y colour)
                                  (let ((glyph (gethash char *glyphs*)))
                                    (when (null glyph)
                                      (error "Error: No glyph for character ~A." char))
                                    (cairo:move-to x y)
                                    (loop for line in glyph
                                          for row from 4 downto 0
                                          do (loop for c across line
                                                   for column from 0 upto 6
                                                   when (equal c #\*)
                                                     do (progn
                                                          (cairo:rectangle (+ x column) (+ y row) 1 1)
                                                          (cairo:set-source-color colour)
                                                          (cairo:fill-path))))))
                                (draw-string (string x y colour)
                                  (loop for char across string
                                        for i from 0
                                        do (progn
                                             (draw-glyph char x y colour)
                                             (incf x 1)
                                             (incf x (glyph-width (gethash char *glyphs*)))))))

                         ;; Grey background
                         (cairo:rectangle 0 0 64 64)
                         (cairo:set-source-color cl-colors:+grey15+)
                         (cairo:fill-path)

                         ;; Time
                         (multiple-value-bind
                               (second minute hour day month year day-of-week dst-p tz)
                             (get-decoded-time)
                           (let* ((day-name (nth day-of-week *day-names*))
                                  (day-name-width (string-width day-name))
                                  ;; (date (format nil "~A ~A" (nth month *month-names*) day))
                                  (date (format nil "~A-~A" month day))
                                  (date-width (string-width date))
                                  (time (format nil "~2,'0d:~2,'0d" hour minute))
                                  (time-width (string-width time)))
                             (draw-string date 2 2 cl-colors:+darkgrey+)
                             (draw-string time (+ 2 date-width (- (round (- 64 day-name-width date-width 4) 2) (round time-width 2))) 2 cl-colors:+lightgrey+)
                             (draw-string day-name (- 63 day-name-width) 2 cl-colors:+darkgrey+)))

                         (cairo:rectangle 0 9 64 9)
                         (let* ((s (seconds-util-next-call))
                                (s-string (format nil "~Ah~Am" (floor s 3600) (floor (mod s 3600) 60)))
                                (next-string (format nil "NEXT:   ~A" s-string))
                                (next-width (string-width next-string)))
                           (cond
                             ((<= s 0)
                              (cairo:set-source-color cl-colors:+red+)
                              (cairo:fill-path)
                              (draw-string "CALL NOW" (round (- 64 (string-width "CALL NOW")) 2) 11 cl-colors:+white+))
                             ((<= s 120)
                              (cairo:set-source-color cl-colors:+red+)
                              (cairo:fill-path)
                              (draw-string (format nil "NEXT:   ~A" s-string) (round (- 64 next-width) 2) 11 cl-colors:+white+))
                             ((<= s 300)
                              (cairo:set-source-color cl-colors:+yellow+)
                              (cairo:fill-path)
                              (draw-string (format nil "NEXT:   ~A" s-string) (round (- 64 next-width) 2) 11 cl-colors:+black+))
                             (t
                              (cairo:set-source-color cl-colors:+seagreen+)
                              (cairo:fill-path)
                              (draw-string (format nil "NEXT:   ~A" s-string) (round (- 64 next-width) 2) 11 cl-colors:+midnightblue+))))

                         ;; Render networth graph
                         (let ((img (opticl:read-png-file "greenbooks/pixoo-ledger.png")))
                           (opticl:with-image-bounds (height width)
                                img
                             (loop for i below height
                                   do (loop for j below width
                                            do (multiple-value-bind (r g b)
                                                   (opticl:pixel img i j)
                                                 (cairo:rectangle j (+ 18 i) 1 1)
                                                 (cairo:set-source-rgb (/ r 255.0) (/ g 255.0) (/ b 255.0))
                                                 (cairo:fill-path))))))
                         ))

                  (cairo:destroy ctx))

              (cairo:surface-write-to-png surface "/tmp/image.png")
              (let ((data (cairo:image-surface-get-data surface)))
                (loop for i from 0 upto (1- (length data)) by 4
                      with pixels = (list)
                      do (progn
                           (push (aref data (+ i 2)) pixels)
                           (push (aref data (+ i 1)) pixels)
                           (push (aref data (+ i 0)) pixels))
                      finally (return (base64:usb8-array-to-base64-string
                                       (coerce (reverse pixels) 'vector))))))))

      (let ((json (cl-json:encode-json-plist-to-string
                   (list :Command "Draw/SendHttpGif"
                         :PicNum 1
                         :PicWidth 64
                         :PicOffset 0
                         :PicID 1
                         :PicSpeed 1
                         :PicData pic-data))))
        (handler-case
            (let ((pixoo-api-endpoint (get-secret :pixoo-api-endpoint)))
              (sb-sys:with-deadline (:seconds 10)
                (log:info "posting Draw/ResetHttpGifId to ~A" pixoo-api-endpoint)
                (drakma:http-request pixoo-api-endpoint
                                     :method :post
                                     :content-type "application/json"
                                     :content (cl-json:encode-json-plist-to-string
                                               (list :Command "Draw/ResetHttpGifId")))
                (log:info "posting image to ~A" pixoo-api-endpoint)
                (drakma:http-request pixoo-api-endpoint
                                     :method :post
                                     :content-type "application/json"
                                     :content json)))
          (sb-sys:deadline-timeout (e)
            (log:error "Timeout posting to pixoo" e)))
        (log:info "done updating pixoo")))))
