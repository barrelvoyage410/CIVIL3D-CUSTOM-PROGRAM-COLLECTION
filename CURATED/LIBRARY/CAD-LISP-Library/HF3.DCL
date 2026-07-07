hf3:dialog {
initial_focus = "CREATE_FACE";
label="3D Face Generator";
:row {
	:button {
		   is_default=true;
		   fixed_width=true;
		   label = "CREATE";
		   key = "CREATE_FACE";
		   mnemonic = "C";
		  }
	:button {
		   is_default=true;
		   fixed_width=true;
		   label = "OPTIONS";
		   key = "HF_OPTIONS";
		   mnemonic = "O";
		  }
	:button {
		   fixed_width=true;
		   label = "TOOLS";
		   key = "HF_TOOLS";
		   mnemonic = "T";
		  }
	:button {
		   is_default=true;
		   fixed_width=true;
		   label = "HELP";
		   key = "HF_HELP";
		   mnemonic = "H";
		  }
	:button {
		   is_default=true;
		   fixed_width=true;
		   label = "EXIT";
		   key = "HF_EXIT";
		   mnemonic = "E";
		  }
}
}

HF_CREATOR: dialog {
initial_focus = "HF_ALL";
label="Create 3D Faces";
	:boxed_radio_row {
	label="Method:";
	:radio_button {
	label = "All";
	mnemonic="A";
	key="HF_ALL";
	}
	:radio_button {
	label = "Layer";
	mnemonic="L";
	key="HF_LAYER";
	}
	:radio_button {
	label = "Sel.";
	mnemonic="S";
	key="HF_SELECT";
	}
}
	:concatenation {
		    : text_part {
		    label = " ";
		    }
		    : text_part {
		    key = "HF_TYPE";
		    width=40;
			    }
    }

	:row {
	alignment=centered;
	:button {
	fixed_width=true;
	label="CREATE";
	mnemonic="C";
	key="HF_CREATE";
	}
	:button {
	fixed_width=true;
	label="HELP";
	key="HF_HELP";
	mnemonic="H";
	}
	cancel_button;
}
}

HF_OPTION:dialog {
initial_focus = "HF_SCALE";
label="Options";
	:column {
	label="OPTIONS:";
           
       // :boxed_column {
	  :edit_box {
	   label="Layer:";
	   mnemonic= "a";
	   key="HF_LAYER";
	   edit_width=8;
			}

	  :edit_box {
	   label="Scale:";
	   mnemonic= "c";
	   key="SCALE";
	   edit_width=4;
			}
	  :edit_box {
	   label="Hface Elev:";
	   mnemonic= "E";
	   key="ELEV";
	   edit_width=4;
			}
:row {
	   :toggle {
	    label = "Layer Off";
	    mnemonic = "L";
	    key = "HIDER";
		}
	:toggle {
	    label = "DimTxt";
	    mnemonic = "D";
	    key = "DTXT";
		}
}
:row {
	    :toggle {
	    alignment="left";
	    label = "Attribute";
	    mnemonic = "A";
	    key = "ATTR";
		}
	    :toggle {
	     alignment="right";
	     label="Mtext";
	     mnemonic="M";
	     key="MTXT";
	     }
}
}
:column {
  :text {
	label="  Configured by:   ";
	edit_width=11;
	alignment = centered;
	  }
:text {
	alignment = centered;
	label= "This is an old version!";
	key = "SAVE_NAME";
	edit_width = 11;
	edit_limit = 11;
	}

	}



:row {
	:button {
	label = "OK";
	key = "YES";
	mnemonic = "O";
	}
	:button {
	label = "HELP";
	key = "HF_HELP";
	mnemonic = "H";
	}
cancel_button;
}
}


HF_SELECTED:dialog {
label = "VERIFY";
initial_focus="VERIFIED";
 :concatenation {
		    : text_part {
		    label = "Selected: ";
		    }
		    : text_part {
		    key = "count";
		    width=30;
			    }
    }
:row {
	:button {
	label = "SELECT";
	key = "HF_SELECT2";
	mnemonic = "S";
	}
	:button {
	label = "HELP";
	key = "HF_HELP";
	mnemonic = "H";
	}
		:button {
	label = "Cancel";
	key = "HF_CANCEL";
	mnemonic = "C";
	}
ok_button;
}
}

HF_TOOL:dialog {
initial_focus = "PREP_PLOT";
label="HFACE TOOLS";
:column {
:row {
	:button {
		   is_default=true;
		   fixed_width=true;
		   label = "Cleanup";
		   key = "HF_CLEAN";
		   mnemonic = "C";
		  }
	:button {
		   is_default=true;
		   fixed_width=true;
		   label = "Pplot";
		   key = "PREP_PLOT";
		   mnemonic = "P";
		  }
	:button {
		   fixed_width=true;
		   label = "Rid";
		   key = "HF_RID";
		   mnemonic = "R";
		  }
}
}
:column {
  :row{
		:button {
		   label = " Library";
		   key = "LISP_LIB";
		   mnemonic = "L";
		  }
	:button {
		   is_default=true;
		   fixed_width=true;
		   label = "Help ";
		   key = "HF_HELP";
		   mnemonic = "H";
		  }
	:button {
		   is_default=true;
		   fixed_width=true;
		   label = "Exit";
		   key = "HF_EXIT";
		   mnemonic = "E";
		  }
}
}
}

HF_HELPER:   dialog {
	label= "HELP";
	: list_box {
		   key = "info";
		   width =65;
		   height = 12;
		   }
      :row {
	 :button {
	   key = "HF_INFO";
	   label = "Information";
	   mnemonic="I";
	   fixed_width=true;
	  }
	 ok_button;
	  }
     }

RID:dialog {
initial_focus = "RID_EM";
label="ERASE HFACES";
:column {
:boxed_row {
      :radio_button {
	     label="All";
	     mnemonic="A";
	     key="RID_ALL";
	     }
       :radio_button {
	    label = "E.Layer";
	    mnemonic = "L";
	    key = "ELAYER";
		}
	  :radio_button {
	    label = "H.Layer";
	    mnemonic = "H";
	    key = "HLAYER";
		}
	    :radio_button {
	    label = "Elev.";
	    mnemonic = "E";
	    key = "RID_ELEV";
		}
}
	    :concatenation {
		    : text_part {
		    label = " ";
		    }
		    : text_part {
		    key = "RID_MESSAGE";
		    width=40;
			    }
			    }
:row {

	:button {
	label = "RID";
	key = "RID_EM";
	mnemonic = "R";
	}
	:button {
	label = "HELP";
	key = "HF_HELP";
	mnemonic = "H";
	}
cancel_button;
}
}
}

info:   dialog {
	:row {
	: image {
		   key = "view";
		   aspect_ratio = 1.5;
		   width = 15;
		   height = 7.5;
		   alignment = left;
		  }

	label= "INFORMATION";
	: list_box {
		   key = "info";
		   width = 50;
		   height = 12;
		   }
	   }
	 ok_button;
     }

	
