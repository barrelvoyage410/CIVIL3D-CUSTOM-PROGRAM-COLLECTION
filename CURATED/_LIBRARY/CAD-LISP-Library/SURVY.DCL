SURVEY: dialog {
initial_focus = "AL_VAR";
label="Survey Tools";
:boxed_column {
 :row {
     :column {
	 :toggle {
	label = "All";
	key = "AL_VAR";
	mnemonic = "A";
	}
	 :toggle {
	label = "DW";
	key = "DW_VAR";
	mnemonic = "D";
	}
	 :toggle {
	label = "FL";
	key = "FL_VAR";
	mnemonic = "F";
	}
      }
   :column{
	 :toggle {
	label = "NUT";
	key = "NUT_VAR";
	mnemonic = "N";
	}
	  :toggle {
	label = "STA";
	key = "STA_VAR";
	mnemonic = "S";
	}
       }
     
}
}
:boxed_column {
        :edit_box {
	   mnemonic= "N";
	   key="NORTH_ROT";
	   edit_width=14;
           alignment = left;
           label="North Rot (Dec. Deg.):";
			}
        
       :edit_box {
           alignment = right;
           label = "Base Pt (x,y,z):";
	   //mnemonic= "B";
	   key="Rot_base";
	   edit_width=14;
			}
      :edit_box {
           alignment = right;
           label = "Block Scale:";
	   //mnemonic= "S";
	   key="Bl_Scale";
	   edit_width=14;
			}
}

:boxed_column {
:row {
	  :toggle {
	label = "Fl=Mh";
	key = "FL_MH";
	mnemonic = "F";
	}
          :toggle {
	label = "Nut=Wv";
	key = "NT_WV";
	mnemonic = "N";
	}
  }
}

   :row {
	:button {
		   is_default=true;
		   fixed_width=true;
		   label = "Create";
		   key = "CREATE_DWG";
		   mnemonic = "C";
		  }
	:button {
		   is_default=true;
		   fixed_width=true;
		   label = "Cancel";
		   key = "Exit";
		   mnemonic = "A";
		  }
	:button {
		   fixed_width=true;
		   label = "Help";
		   key = "SVY_HELP";
		   mnemonic = "H";
		  }
       }//END ROW
}//end box




HELP_SUR:   dialog {
	label= "HELP";
	: list_box {
		   key = "SUR_HELP";
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
