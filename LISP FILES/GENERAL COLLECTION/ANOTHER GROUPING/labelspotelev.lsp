(defun c:lse ( / )
  (command "-vbarun" "label_spot.dvb!LabelSpotElevation.LabelSpotElev")
  (princ)
)