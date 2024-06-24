(in-package :aon)

(defvar *timeout-count* 0)

(defun generate-asset-png ()

  (let ((start-date (adjust-timestamp (now) (offset :day -14)))
        (tomorrow (adjust-timestamp (now) (offset :day 1))))

    ;; Update ledger git repos
    (handler-case
        (progn
          (legit:with-chdir ("greenbooks/")
            (legit:git-config :name "pull.rebase" :value "true")
            (legit:pull "."))
          (legit:with-chdir ("greenbooks/daily-price-depot/")
            (legit:git-config :name "pull.rebase" :value "true")
            (legit:pull "."))
          (setf *timeout-count* 0))
      (usocket:timeout-error (e)
        (incf *timeout-count*)
        (when (> *timeout-count* 3)
          (log:error "Timeout connecting to greenbooks repo ~A times" *timeout-count*)))
      (error (e)
        (log:error "Unhandled condition!! ~A" e)))

    (uiop:with-current-directory (#p"greenbooks/")

      (let* ((ledger-data (loop for date = start-date then (adjust-timestamp date (offset :day 1))
                                until (timestamp> date tomorrow)
                                collect (multiple-value-bind
                                              (second minute hour day month year day-of-week dst-p tz)
                                            (decode-universal-time (timestamp-to-universal date))
                                          (let* ((cmd (format nil "ledger -f book.ledger --price-db prices.db -E -e ~A-~A-~A --collapse -X $ bal ^assets"
                                                              year month day))
                                                 (line (car (inferior-shell:run/lines cmd))))
                                            (cl-ppcre:register-groups-bind (value-with-commas) (".*\\$([^ ]+).*" line)
                                              (let* ((value (parse-integer (remove #\, value-with-commas) :junk-allowed t)))
                                                (cons (format nil "~A-~A-~A" year month day) value)))))))
             (max-value (loop for entry in ledger-data
                              maximize (cdr entry)))
             (min-value (loop for entry in ledger-data
                              minimize (cdr entry)))
             (data-file (fad:with-output-to-temporary-file (stream)
                          (dolist (entry ledger-data)
                            (format stream "~A   ~A~%" (car entry) (cdr entry))))))

        (eazy-gnuplot:with-plots (*standard-output* :debug t)
          (eazy-gnuplot:gp-setup :output #p"pixoo-ledger.png"
                                 :terminal '(:png :size :|64,46|)
                                 :xdata :time
                                 :timefmt '"%Y-%m-%d"'
                                 :yrange (list min-value max-value)
                                 :lmargin '(at screen 0)
                                 :rmargin '(at screen 1)
                                 :tmargin '(at screen 1)
                                 :bmargin '(at screen 0)
                                 :object '(1 rect from screen :|0,0| to screen :|1,1| behind)
                                 :object '(1 rect fc rgb "#222222" fillstyle solid 1.0)
                                 :style '(fill solid 1.0 border rgb "#181818"))
          (eazy-gnuplot:gp :unset :border)
          (eazy-gnuplot:gp :unset :xtics)
          (eazy-gnuplot:gp :unset :ytics)
          (eazy-gnuplot:gp :unset :key)
          (eazy-gnuplot:gp :set :ytics (parse-integer (get-secret :pixoo-assets-ytics)))
          (eazy-gnuplot:gp :set :grid :ytics :lc :rgb "#303030")
          (eazy-gnuplot:plot
           (lambda ()
             (loop for row in (uiop:read-file-lines data-file)
                   do (format t "~A~%" row)))
           :using '(1 2)
           :with '(filledcurves below x1 lc rgb "#333333")))

        ;; Delete the temporary file
        (delete-file data-file)))))
