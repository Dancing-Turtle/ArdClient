(in-package :hlu)

(deflayer audio2 (t)
  (lambda (msg fn out)
    (decoder msg out
      (let ((ver :uint8 "version (1 or 2)"))
        (if (or (= ver 1)
                (= ver 2))
            (progn
              :string "id"
              (when (= ver 2)
                :uint16 "volume (version = 2 only)")
              :raw :all (strcat fn ".ogg"))
            (error "Invalid versionf or aduio2")))))
  (lambda (fn out in)
    (encoder in out
      (let ((ver :uint8))
        (if (or (= ver 1)
                (= ver 2))
            (progn
              :string
              (when (= ver 2)
                :uint16)
              :raw (strcat fn ".ogg"))
            (error "Invalid version in audio2"))))))
