// RPR of National Survey & Engineering 9/13/95

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

slope : dialog {
  label                 = "Slope Maker";
  initial_focus         = "accept";
  LOGO_row;
  : spacer { height = 1.5; }
  : edit_box {
    label               = "Offset Distance: ";
    mnemonic            = "O";
    key                 = "off_set";
    value               = "1.0";
    edit_width          = 10;
    alignment           = right;
    fixed_width         = true;
  }
  : edit_box {
    label               = "Change in Elevation: ";
    mnemonic            = "E";
    key                 = "ch_elev";
    value               = "1.0";
    edit_width          = 10;
    alignment           = right;
    fixed_width         = true;
  }
  : spacer { height = 1.5; }
  ok_cancel;
  errtile;
}
