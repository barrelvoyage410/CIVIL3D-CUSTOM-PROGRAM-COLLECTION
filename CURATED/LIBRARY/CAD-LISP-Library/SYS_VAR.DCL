
SYS_VAR:dialog {
    initial_focus = "VAR_SAVE";
    label = "System Variable Saver";
     :row {
	:button {
		   is_default=true;
		   fixed_width=true;
		   label = "SAVE";
		   key = "VAR_SAVE";
		   mnemonic = "S";
		  }
	:button {
		   fixed_width=true;
		   label = "RESTORE";
		   key = "VAR_REST";
		   mnemonic = "R";
		  }
	:button {
		   fixed_width=true;
		   label = "HELP";
		   key = "VAR_HELP";
		   mnemonic = "H";
		  }
	cancel_button;
}
}

SYS_SAVE: dialog {
initial_focus = "AL_VAR";
label="Save Variables";
:boxed_column {
 :row {
     :column {
	 :toggle {
	label = "All";
	key = "AL_VAR";
	mnemonic = "A";
	}
	 :toggle {
	label = "Attrib.";
	key = "AT_VAR";
	mnemonic = "t";
	}
	 :toggle {
	label = "Dims";
	key = "DI_VAR";
	mnemonic = "D";
	}
	 :toggle {
	label = "Grips";
	key = "GR_VAR";
	mnemonic = "G";
	}
	  :toggle {
	label = "Interf.";
	key = "IN_VAR";
	mnemonic = "I";
	}
       }
    :column {
	:toggle {
	label = "Modes";
	key = "MO_VAR";
	mnemonic = "M";
	}
	:toggle {
	label = "Picks";
	key = "PI_VAR";
	mnemonic = "i";
	}
	:toggle {
	label = "Points";
	key = "PO_VAR";
	mnemonic = "P";
	}
	:toggle {
	label = "Sorts";
	key = "SO_VAR";
	mnemonic = "o";
	}
	:toggle {
	label = "Unit/Line";
	key = "UN_VAR";
	mnemonic = "U";
	}
	  }
 }
}
 :column {
  :text {
	label="  Filename:   ";
	edit_width=11;
	alignment = centered;
	  }
  :text {
	alignment = centered;
	label= "File";
	key = "SAVE_FILE";
	edit_width = 11;
	edit_limit = 11;
	}

:text {
	alignment = centered;
	label= " ";
	key = "SAVE_NAME";
	edit_width = 11;
	edit_limit = 11;
	}

	}
   :row {
	    :button {
	label="SAVE";
	key = "S_VARS";
	mnemonic = "S";
			 }
	 cancel_button;
}
}




help_VAR:   dialog {
	label= "HELP";
	: list_box {
		   key = "VAR_HELP";
		   width = 65;
		   height = 12;
		   }
	 ok_button;
     }

Chk_Write: dialog {
	label="File exists! Overwrite?";
	  :row {
	:button {
		   is_default=false;
		   fixed_width=true;
		   label = "Yes";
		   key = "CHK_YES";
		   mnemonic = "Y";
		  }
	:button {
		   is_default=true;
		   fixed_width=true;
		   label = "No";
		   key = "CHK_NO";
		   mnemonic = "N";
		  }
    }
}
