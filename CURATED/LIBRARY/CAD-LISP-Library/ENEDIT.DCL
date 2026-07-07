edit:   dialog {
        label= "EnEdit";
  :row {
        :column {  
        :boxed_column {
        :button {
        label = "Select Sample Entity <";
        mnemonic = "S";
        key = "Sel_Ent";
                }


    :popup_list{
        label = "Edit:";
        key = "edit";
        value="2";
          }
 }
  }

   :boxed_column {

    :row{
        :popup_list{
        label = "SS by: ";
        key = "type";
        value="1";
    }   
        : text {
	      width = 8;
	      fixed_width = false;
	      key = "FILT_1";
	      }   
    }   

   :row{
        :popup_list{
        label = "And: ";
        key = "typtw";
        value="3";
    } 
	: text {
	      width = 8;
	      fixed_width = false;
	      key = "FILT_2";
	      }
}        

    :row {
        :popup_list{
        label = "And:";
        key = "typthr";
        value="4";
          }
       : text {
	      width = 8;
	      fixed_width = false;
	      key = "FILT_3";
	      }
}


 }
 }
spacer;
spacer;
         :row {
          alignment = centered;
           :button {
                   fixed_width=true;
                   label = "INFO..";
                   key = "info";
                   mnemonic = "I";
                  }
          :spacer {width = 1;}
           :button {
                   fixed_width=true;
                   label = "HELP";
                   key = "help";
                   mnemonic = "H";
                  }
          :spacer {width = 1;}
           :button {
                   is_default=true;
                   fixed_width=true;
                   label = "Edit";
                   key = "accept";
                   mnemonic = "E";
                  }
          :spacer {width = 1;}
         cancel_button;
         //errtile;
     
        }
  }

info:   dialog {
        :row {
        : image {
                   key = "view";
                   width = 15;
                   height = 5;
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

help:   dialog {
        label= "HELP";
        : list_box {
                   key = "help";
                   width = 50;
                   height = 12;
                   }
         ok_button;
     }
pline:   dialog {
        label= "POLYLINE EDITOR";
  :boxed_column {
    :edit_box {
    label="New Width";
    mnemonic="W";
    key="nw_width";
    edit_width=5;
               }
    :edit_box {
    label="New Size";
    mnemonic="w";
    key="nw_sz";
    edit_width=4;
                }
    :edit_box {
    label="New Style";
    mnemonic="y";
    key="nw_style";
    edit_width=10;
    }
  }
}
block:   dialog {
        label= "BLOCK EDITOR";
  :boxed_column {
    :edit_box {
    label="Scale:";
    mnemonic="S";
    key="scale";
    edit_width=4;
                }
    :edit_box {
    label="Rotation:";
    mnemonic="R";
    key="rotate";
    edit_width=8;
                }
    :edit_box {
    label="New Block:";
    mnemonic="N";
    key="new";
    edit_width=8;
                }
       }
       ok_button;
       }

textx:   dialog {
        label= "TEXT EDITOR";
  :boxed_column {
    :edit_box {
    label="New Text";
    mnemonic="T";
    key="nw_txt";
    edit_width=10;
               }
    :edit_box {
    label="New Size";
    mnemonic="w";
    key="nw_sz";
    edit_width=4;
                }
    :edit_box {
    label="New Style";
    mnemonic="y";
    key="nw_style";
    edit_width=10;
    }
  }
}







