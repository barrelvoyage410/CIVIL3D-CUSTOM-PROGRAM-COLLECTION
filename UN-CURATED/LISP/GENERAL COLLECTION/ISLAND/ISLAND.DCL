// RPR of National Survey & Engineering 10/20/95

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

island : dialog {
  label               = "Island Maker";
  initial_focus       = "accept";
  LOGO_row;
  : spacer { height = 1.5; }
  : row {
    : boxed_row {
      label               = "Parking Angle (Choose One): ";
      mnemonic            = "A";
      key                 = "isleangpar";
      fixed_height        = true;
      alignment           = top;
      : radio_column {
        height              = 3;
        fixed_height        = true;
        : radio_button {
          key                 = "baseang";
          value               = "1";
        }
        : radio_button {
          key = "compang";
        }
      }
      : column {
        : edit_box {
          label               = "Base Parking Angle: ";
          mnemonic            = "B";
          key                 = "isleang";
          value               = "90.0";
          edit_width          = 5;
          alignment           = right;
          fixed_width         = true;
        }
        : edit_box {
          label               = "Complimentary Angle: ";
          mnemonic            = "C";
          key                 = "islecang";
          value               = "90.0";
          edit_width          = 5;
          alignment           = right;
          fixed_width         = true;
        }
      }
    }
    : spacer { height = 1.5; }
    : boxed_column {
      label               = "Island Parameters";
      mnemonic            = "I";
      key                 = "islepar";
      : edit_box {
        label               = "Minimum Island Width: ";
        mnemonic            = "M";
        key                 = "islemin";
        value               = "6.0";
        edit_width          = 5;
        alignment           = right;
        fixed_width         = true;
      }
      : edit_box {
        label               = "Aisle Access Radius: ";
        mnemonic            = "R";
        key                 = "islera";
        value               = "6.0";
        edit_width          = 5;
        alignment           = right;
        fixed_width         = true;
      }
      : edit_box {
        label               = "Stall Return Radius: ";
        mnemonic            = "S";
        key                 = "islerr";
        value               = "2.0";
        edit_width          = 5;
        alignment           = right;
        fixed_width         = true;
      }
    }
  }
  : boxed_column {
    label               = "Parking Stall Parameters";
    mnemonic            = "P";
    key                 = "stallpar";
    fixed_height        = true;
    alignment           = top;
    : edit_box {
      label               = "Stall Length (Perpendicular to aisle C/L): ";
      mnemonic            = "L";
      key                 = "islelen";
      value               = "18.0";
      edit_width          = 5;
      alignment           = right;
      fixed_width         = true;
    }
    : edit_box {
      label               = "Stall Stripe Length: ";
      mnemonic            = "S";
      key                 = "stalllen";
      value               = "18.0";
      edit_width          = 5;
      alignment           = right;
      fixed_width         = true;
    }
    : edit_box {
      label               = "Stall Width (Minimum): ";
      mnemonic            = "W";
      key                 = "stallwid";
      value               = "9.0";
      edit_width          = 5;
      alignment           = right;
      fixed_width         = true;
    }
    : edit_box {
      label               = "Stall Offset (Width along aisle C/L): ";
      mnemonic            = "O";
      key                 = "stalloff";
      value               = "9.0";
      edit_width          = 5;
      alignment           = right;
      fixed_width         = true;
    }
  }
  : spacer { height = 1.5; }
  ok_cancel;
  errtile;
}
