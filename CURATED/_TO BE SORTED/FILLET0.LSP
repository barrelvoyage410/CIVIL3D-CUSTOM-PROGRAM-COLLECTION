(defun c:fz ()     ; line 1
  (setq old_radius (getvar "filletrad"))     ; line 2
  (setvar "filletrad" 0)     ; line 3
  (command ".fillet" pause pause)     ; line 4
  (setvar "filletrad" old_radius)     ; line 5
)     ; line 6

