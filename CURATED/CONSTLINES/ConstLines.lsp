;|
ConstLines.lsp [command name: RX]
To draw Construction Lines [Rays and/or Xlines] with options in combined command.
Draws on dedicated construction-lines Layer [see comments below].
Initial defaults:  Xline, at Free direction.
Options:
  Change between Xline & Ray within one command;
  Change direction between Free [User on-screen pick each time], Angle [User-specified
    typically non-orthogonal direction, though it can be set same as any built-in one], and
    preset orthogonal directions -- if Xline, Horizontal or Vertical, or if Ray, East/North/
    West/South from User-selected point(s);
  Undo last-drawn object.
Undo option removes latest one(s) as desired back to beginning of command, however
  if repeated Undo options pass a point of change of other option(s) [of direction and/or
  entity type], does not reset those option changes, but continues to use settings that
  were current before Undo option(s).
NOTE that Undo option within command removes latest one only, whereas Undo after
  completion of command removes all Rays and/or Xlines drawn within it.
Continuously draws Xlines/Rays through specified points at current settings until User
  chooses option to change setting, then continues under new settings.  Displays current
  settings at Command line above pick-an-origin prompt, offering options appropriate
  for changing current settings.
Remembers directional choices for Xline and Ray separately.  Global variables:
  *RXcom = COMmand name [Xline/Ray]
  *RXXdir = Xline DIRection [Free/Horiz/Vert/Angle]
  *RXRdir = Ray DIRection [Free/East/North/West/South/Angle]
  *RXXang = Xline ANGle [requested & used only under Angle option]
  *RXRang = Ray ANGle [as for Xline angle]
Remembers User-Angle value for each even if direction changed to other option(s), for
  possible return to Angle option.
Uses compass directions for Ray orthogonal-direction options, rather than Right/Up/
  Left/Down, to avoid any confusion, or requirement for two-capital-letter keyword
  for one of them, between Up and Undo, or [even though never offered at the same
  time] between Right and Ray.
Enter and space [for <exit> option] and Escape end it, and reset previous current Layer.
Kent Cooper, last edited 18 July 2016
|;

(defun C:RX ; = Ray/Xline combined construction-line command
  (/ *error* var ev doc svnames svvals n done pt)

  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); if
    (mapcar 'setvar svnames svvals)
    (vla-endundomark doc)
    (princ)
  ); defun - *error*

  (defun var (opt); construct variable name for current type
    (read (strcat "*RX" (substr *RXcom 1 1) opt))
  ); defun -- var

  (defun ev (opt); get value from variable for current type
    (eval (var opt))
  ); defun -- ev

  (vla-startundomark (setq doc (vla-get-activedocument (vlax-get-acad-object))))
  (setq
    svnames '(clayer cmdecho blipmode)
    svvals (mapcar 'getvar svnames)
    n 0; counter for Undo option
  ); setq
  (setvar 'cmdecho 0)
;|
Omit/comment-out following code lines to use current Layer, or EDIT Layer name
  & properties, add other options [e.g. linetype], etc., as desired.  [If eliminated, no
  need to include clayer in svnames variable list.]
|;
  (command "_.layer"
    "_thaw" "A-CONS-LINE" "_make" "A-CONS-LINE" ; [thaw in case it exists but is frozen]
    "_color" 234 "" "_plot" "_no" "" ""
  ); command
  (foreach pair '((*RXcom "Xline") (*RXXdir "Free") (*RXRdir "Free"))
    (if (not (eval (car pair))) (set (car pair) (cadr pair))); initial defaults
  ); foreach
  (while (not done)
    (prompt
      (strcat
        "\nCurrent construction-line settings: " *RXcom ", "
        (if (= (ev "dir") "Angle")
          (strcat (angtos (ev "ang")) " Angle"); then
          (strcat (ev "dir") " direction"); else
        ); if
        "."
      ); strcat
    ); prompt
    (initget
      (strcat
        (if (= *RXcom "Xline")
          (strcat ; then
            (vl-string-subst "" (strcat *RXXdir " ") "Free Horiz Vert ")
            "Angle Ray"
          ); strcat
          (strcat ; else [currently Ray]
            (vl-string-subst "" (strcat *RXRdir " ") "Free East North West South ")
            "Angle Xline"
          ); strcat
        ); if
        (if (> n 0) " Undo" ""); Undo option only if things to undo
      ); strcat
    ); initget
    (setq pt
      (getpoint
        (strcat
          "\nPick " *RXcom " origin point or ["
          (if (= *RXcom "Xline")
            (strcat ; then
              (vl-string-subst "" (strcat *RXXdir "/") "Free/Horiz/Vert/")
              "Angle/Ray"
            ); strcat
            (strcat ; else [currently Ray]
              (vl-string-subst "" (strcat *RXRdir "/") "Free/East/North/West/South/")
              "Angle/Xline"
            ); strcat
          ); if
          (if (> n 0) "/Undo" ""); Undo option only if things to undo
          "] <exit>: "
        ); strcat
      ); getpoint
    ); setq [pt]
    (cond
      ( (not pt) (setq done T)); Enter/space [nil] -- <exit> default
        ;; [put before picked-a-point condition because (listp nil) returns T]
      ( (listp pt); picked a point
        (command (strcat "_." *RXcom)); Ray/Xline command
        (if (= (ev "dir") "Free")
          (setvar 'cmdecho 1); then -- prompt for through-point after pt fed to command
          (setvar 'blipmode 0); else -- suppress [if used] for through-point fed by routine
        ); if
        (command "_none" pt)
        (if (= (ev "dir") "Free") (setvar 'cmdecho 0)); [through-point prompt already there]
        (cond ; direction?
          ( (= (ev "dir") "Free") (command pause))
          ( (= (ev "dir") "Angle") (command "_none" (polar pt (ev "ang") 1)))
          ( T ; other than Free or Angle
            (command "_none"
              (polar
                pt
                (* pi ; angle
                  (cadr
                    (assoc (substr (ev "dir") 1 1)
                      '(("E" 0) ("H" 0) ("N" 0.5) ("V" 0.5) ("W" 1) ("S" 1.5))
                    ); assoc
                  ); cadr
                ); *
                1 ; distance
              ); polar
            ); command
          ); built-in orthogonal-direction condition
        ); cond
        (command "") ; end Ray/Xline
        (setvar 'blipmode (last svvals)); restore [if used] for next pt pick
        (setq n (1+ n)); for Undo option
      ); picked-a-point condition
      ( (= pt "Angle")
        (set (var "dir") pt)
        (initget (if (ev "ang") 0 1)); no Enter on first use
        (set (var "ang")
          (cond
            ( (getangle
                (strcat
                  "\nAngle for " *RXcom
                  (if (ev "ang")
                    (strcat " <" (angtos (ev "ang")) ">"); then [in current angular settings]
                    "" ; else [no default if not yet used]
                  ); if
                  ": "
                ); strcat
              ); getangle
            ); User input condition
            ((ev "ang")); prior value if present
          ); cond
        ); set
      ); User-designated [typically non-orthogonal] Angle condition
      ( (wcmatch pt "Xline,Ray")
        (setq *RXcom pt)
      ); other-command-name condition
      ( (= pt "Undo")
        (command "_.u"); remove previous one
        (setq n (1- n))
      ); Undo condition
      ( (set (var "dir") pt)); other built-in direction option [F/E/H/N/V/W/S]
    ); cond
  ); while
  (mapcar 'setvar svnames svvals)
  (vla-endundomark doc)
  (princ)
); defun -- C:RX

(vl-load-com)
(prompt "\nType RX for combined Ray/Xline construction-line(s) command.")