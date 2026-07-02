// RPR of National Survey & Engineering 2/29/96

dcl_settings : default_dcl_settings {
  audit_level           = 0;
}

LOGO_row : boxed_row {
  alignment             = centered;
  fixed_width           = true;
  : image {
    key                 = "LOGO_slide";
    color               = dialog_background;
    width               = 30.0;
    height              = 4.0;
  }
}

spot : dialog {
  label                 = "Spot Elevation Settings";
  initial_focus         = "accept";
  LOGO_row;
  : spacer { height = 1; }
  : row {
    : spacer { width = 1; }
    : column {
      : edit_box {
        label               = "&Block Name:";
        key                 = "spotsy";
        value               = "SPOTX";
        edit_width          = 10;
      }
      : edit_box {
        label               = "&Layer:";
        key                 = "spotla";
        value               = "0";
        edit_width          = 10;
      }
      : edit_box {
        label               = "&Scale:";
        key                 = "spotsc";
        value               = "1.0";
        edit_width          = 10;
      }
      : edit_box {
        label               = "&Rotation Angle:";
        key                 = "spotro";
        value               = "0.0";
        edit_width          = 10;
      }
      : edit_box {
        label               = "&Precision:";
        key                 = "spotpr";
        value               = "2";
        edit_width          = 10;
      }
      : spacer { height = 1; }
      : toggle {
        label               = "&Include parentheses around elevation text.";
        key                 = "spotpar";
        value               = "0";
      }
      : toggle {
        label               = "&Append T/C to elevation text.";
        key                 = "spott_c";
        value               = "0";
      }
      : toggle {
        label               = "Prompt for &Text location after insert.";
        key                 = "spotpos";
        value               = "0";
      }
      : toggle {
        label               = "Insert block at &0 (zero) elevation.";
        key                 = "spotel0";
      }
    }
    : spacer { width = 1; }
  }
  : spacer { height = 1; }
  ok_cancel;
  errtile;
}
