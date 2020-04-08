(in-package :stump-volume-control)

(defvar *sound-card* 0
  "audio device to control by amixer; string, number, or null.
  If it is a string, it will be used as a device name as it is,
  if it is a number it will be translated to the string \"hw:0\",
  if NIL don't send any device at all.")
#+nil (setf *sound-card* '(0 (1 "Speaker")))

(defvar *index-of-current-sound-card* 0)

(defun switch-sound-card ()
  (setf *index-of-current-sound-card*
        (if (consp *sound-card*)
            (mod (1+ *index-of-current-sound-card*)
                 (length *sound-card*))
            0)))

(defun get-current-sound-card ()
  (if (consp *sound-card*)
      (nth *index-of-current-sound-card* *sound-card*)
      *sound-card*))

(defun get-current-device (&optional (sound-card (get-current-sound-card)))
  (if (consp sound-card)
      (first sound-card)
      sound-card))

(defun get-current-control (&optional (sound-card (get-current-sound-card)))
  (if (consp sound-card)
      (second sound-card)
      "Master"))

(defvar *pulse-audio-unmute-outputs* '("Speaker" "Headphone")
  "PulseAudio mutes more than it unmutes; when this variable is set,
  the listed audio outputs will explicitly be unmuted when toggling
  audio back on.

  \"Speaker\" and \"Headphone\" are the generic output names but
  some hardware might expose special names.

  Change to NIL if you just use ALSA and do not want the extra calls to
  be executed (which will fail silently if you do not have the outputs).")

(defun translate-device-to-option (device)
  "see docstring of *sound-card*"
  (etypecase device
    (string (format nil "-D ~a " device))
    (number (format nil "-D hw:~d " device))
    (null "")))

(defun make-amixer-call (command &key (device (get-current-device))
                                      (control (get-current-control)))
  (format nil "amixer ~asset ~s ~a"
          (translate-device-to-option device)
          control
          command))

(defcommand volume-up () ()
  (run-shell-command (make-amixer-call "playback 2db+"))
  (message "Audio bit lowder."))

(defcommand volume-down () ()
  (run-shell-command (make-amixer-call "playback 2db-"))
  (message "Audio bit quieter."))

(defcommand volume-toggle-mute () ()
  (let ((muted (search "[off]"
                       (run-shell-command
                        (make-amixer-call "playback toggle")
                        t))))
    (when (not muted)
      (dolist (output *pulse-audio-unmute-outputs*)
        ;; Just unmute all listed outputs explicitly when going back on.
        ;; Of course, we cannot fix PulseAudio with a small hack here.
        (run-shell-command (make-amixer-call "playback on" :control output))))
    (message (if muted
                 "Audio muted."
                 (format nil "Audio back on.~@[~%(Also switched on outputs: ~{~a~^, ~}.)~]"
                         *pulse-audio-unmute-outputs*)))))

(defcommand volume-switch-sound-card () ()
  (switch-sound-card)
  (message (format nil "Switched to sound card ~s and control ~s"
                   (get-current-device)
                   (get-current-control))))
