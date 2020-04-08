(in-package :stump-radio)

;; radio stations

(defvar *stations*
  '((:|Le DJAM| . "http://www.djamradio.com/sound")
    (:|FluxFM| . "http://streams.fluxfm.de/live/mp3-320/audio/")
    (:|1Live| . "https://wdr-1live-live.icecastssl.wdr.de/wdr/1live/live/mp3/128/stream.mp3"))
  "association list of radio stations, key is a name, value is a playable URL of the radio station")

(defun add-station (name url)
  (setf *stations* (acons name url *stations*)))

(defun remove-station (name)
  (setf *stations* (remove name *stations* :key #'car)))

(defun list-stations ()
  (mapcar #'car *stations*))

(defun clear-stations ()
  "clear all stations in case you want to get rid of the defaults"
  (setf *stations* nil))

(defun assert-stations ()
  "make sure *stations* is not empty; to be used in functions that try to access *stations*"
  (assert (consp *stations*) (*stations*) "no radio stations in *stations*"))

(defun next-radio-station ()
  (assert-stations)
  (setf (cdr (last *stations*)) (list (car *stations*))
        *stations* (cdr *stations*))
  (car *stations*))

(defun previous-radio-station ()
  (assert-stations)
  (setf *stations* (cons (car (last *stations*))  *stations*)
        (cdr (last *stations* 2)) nil)
  (car *stations*))

;; radio playing and control

(defvar *radio* nil
  "holds the process structure of mplayer when a radio is running (or at least we think it is running),
  NIL if not playing anything (as it was not yet ever playing or we properly shut it down)")

(defvar *mplayer-termination-signal* sb-unix:sigterm
  "Usually, we terminate mplayer with SIGTERM. If you absolutely want
  instant switching of stations and do not care about letting mplayer
  perperly self-terminate, set the variable to SIGKILL instead:
    (setf *mplayer-termination-signal* sb-unix:sigkill)
  (See also RADIO-STOP.)")

(defvar *radio-debug-level* 5
  "debug level for stump-radio debugging")

(defvar *sent-termination-signal* nil
  "used to detect that a status change to :exited was caused by
  our own intentional process termination")

(defun terminate-radio-and-wait ()
  (setf *sent-termination-signal* t)
  (sb-ext:process-kill *radio* *mplayer-termination-signal*
                       (if (= sb-unix:sigkill *mplayer-termination-signal*)
                           ;; For some streams mplayer might spawn child
                           ;; processes (for example, in case of redirects),
                           ;; so we need to send the signal to whole process
                           ;; group for SIGKILL, as this signal this prevents
                           ;; the process from proper handling the signal itself:
                           :process-group
                           ;; For other signals trust that the process knows
                           ;; best how to handle it and what to do with its
                           ;; child processes:
                           :pid))

  ;; kludge to make RADIO-STOP wait for up to 5 s, hoping that
  ;; the SIGTERM was handled by then and RADIO-STATUS-CHANGE ran.
  ;; Usually, this should spin for just one iteration (.05 s)
  ;; but it should prevent a late status change message
  ;; for RADIO-NEXT-STATION and RADIO-PREVIOUS-STATION which
  ;; would be irritating for the user.
  ;;
  ;; When I multiple times hit the key that executes RADIO-NEXT-STATION
  ;; but with small delays in between, in many tests I could get the wait
  ;; time up to 2.45 s on my system.
  ;; It seems to happen when mplayer ran long enough to start trying
  ;; to connect to the network already. If it receives SIGTERM then,
  ;; it will first wait for the network requests to finsh and only
  ;; terminate afterwards.
  ;; It does not happen when it receives the SIGTERM right away as then
  ;; then the process is terminated by the signal itself which is fast
  ;; (see EXITED-OR-SIGNALED-TERMINATION-P).
  ;;
  ;; See also *MPLAYER-TERMINATION-SIGNAL* if you do not want to wait.
  (when (eq :failed (loop
                       with wait = .05
                       with max-i = 100
                       for i from 1
                       while *radio*
                       do (progn (sleep wait)
                                 (dformat *radio-debug-level*
                                          "RADIO-STOP waiting ~,3f s already~%" (* i wait)))
                       when (>= i max-i)
                       return :failed))
    ;; Waiting might still fail, for example, when someone manually sent
    ;; a SIGSTOP. In that case, the process will be :stopped and stays that way.
    ;; After the timeout, stump-radio forgets about it. But even then, the
    ;; SIGTERM was sent already, so when someone sends a SIGCONT, the process
    ;; right away handles the old SIGTERM and terminates (to status :exited).
    ;; And when a SIGKILL was sent, the process will be terminated right away
    ;; (to status :signaled).
    ;; All that is okay. We don't want to mess with manually sent signals.
    (message "Warning: Waited for radio to stop but stopped waiting after 5 s.")
    (setf *radio* nil)))

(defun exited-or-signaled-termination-p (process)
  "If a process handled SIGTERM correctly it should terminate normally
  which results in process-status :exited.
  But if it was signaled too early it could not yet handle the signal
  and thus it is terminated by the signal itself. In that case, the
  resulting process-status is :signaled with the causing signal as
  exit-code."
  (or (eq :exited (sb-ext:process-status process))
      (and (eq :signaled (sb-ext:process-status process))
           (= *mplayer-termination-signal* (sb-ext:process-exit-code process)))))

(defun radio-running-p ()
  (and *radio*
       ;; don't test simply for :running as this is not binary logic and
       ;; the process might also be in :stopped, :signaled, or :exited
       (not (exited-or-signaled-termination-p *radio*))))

(defun radio-status-change (process)
  (if (and *sent-termination-signal*
           (exited-or-signaled-termination-p process))
      ;; intentional process termination by us detected -> handle
      (progn
        (message "Radio stopped.")
        (setf *radio* nil))
      ;; unknown signal -> show it
      (message (format nil "Radio status changed to ~a.~%(Process ID: ~a)"
                       (sb-ext:process-status process)
                       (sb-ext:process-pid process)))))

(defvar *radio-start/stop-mutex* (sb-thread:make-mutex :name "stump-radio start/stop mutex")
  "mutex to ensure that no calls to RADIO-START and RADIO-STOP overlap")

(defcommand radio-start () ()
  "start radio if not running"
  (sb-thread:with-mutex (*radio-start/stop-mutex* :wait-p nil)
    (assert-stations)
    (if (radio-running-p)
        (message "Warning: radio already running, not starting.")
        (destructuring-bind (name . url)
            (car *stations*)
          (message (format nil "Starting ~a radio..." name))
          (setf *sent-termination-signal* nil
                *radio* (sb-ext:run-program "mplayer" (list url)
                                            :search t
                                            :wait nil
                                            :status-hook #'radio-status-change))))))

(defcommand radio-stop () ()
  "stop radio if running"
  (sb-thread:with-mutex (*radio-start/stop-mutex* :wait-p nil)
    (if (not (radio-running-p))
        (message "Warning: radio not running, not stopping.")
        (progn
          (message "Stopping radio...")
          (terminate-radio-and-wait)))))

(defcommand radio-toggle-playback () ()
  "stop radio if running and start playing if not"
  (if (radio-running-p)
      (radio-stop)
      (radio-start)))

(defcommand radio-force-restart () ()
  "stop current radio and start playing again
  (use for network problems or after suspend)"
  (radio-stop)
  (dformat *radio-debug-level* "RADIO-FORCE-RESTART between stop and start~%")
  (radio-start))

(defcommand radio-next-station () ()
  "switch to next radio station and play that"
  (next-radio-station)
  (radio-force-restart))

(defcommand radio-previous-station () ()
  "switch to previous radio station and play that"
  (previous-radio-station)
  (radio-force-restart))

(defcommand radio-list-stations () ()
  "list radio stations"
  (message (format nil "radio stations: ~{~&  ~a~^,~}" (list-stations))))
