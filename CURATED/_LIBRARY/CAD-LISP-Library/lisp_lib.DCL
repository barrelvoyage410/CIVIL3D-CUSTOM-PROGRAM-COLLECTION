dcl_settings : default_dcl_settings { audit_level = 3; }

kwm_lst: dialog {
    initial_focus = "lsp_type";
    label = "Super Lisp Library";
     :row {
	 :column {
	      :text {
	label= "Type:";
	       }
		    :popup_list {
		 width=22;
		 multiple_select = false;
		 key = "lsp_type";
			 value= "0";
					}  
		   :concatenation {
		    : text_part {
		    label = "Lisps: ";
		    }
		    : text_part {
		    key = "NO_LSPS";
		    width=4;
			    }
			    }
		     :list_box {
			  key = "lisp_list";
		  value = "2";
			  }
	}
   :boxed_column {
		:button {
		      fixed_width=true;
	      label = "Info..";  
	      key = "lsp_info";
	      mnemonic = "I";
			       } 
spacer;  
	   :button {
		   fixed_width=true;
		   label = "HELP";
		   key = "lsp_help";
		   mnemonic = "H";
		  } 
spacer;  
           :button {
                   fixed_width=true;
	           alignment=left;
                   label = "CFG.";
                   key = "lsp_config";
                   mnemonic = "C";
                  }           
spacer; 
	     :button {
		   fixed_width=true;
		   label = "Desc..";
		   key = "lsp_desc";
		   mnemonic = "D";
		  }
spacer;
	:button {
		   fixed_width=true;
		   label = "Load";
		   key = "lsp_load";
		   mnemonic = "L";
		  }
spacer;
	:button {
		   fixed_width=true;
		   label = "Search";
		   key = "lsp_search";
		   mnemonic = "S";
		  }
spacer;
	 ok_button;
}
}
	: text {
	      width = 25;
	      fixed_width = true;
	      key = "MESS_TXT";
	      }
}

lisp_desc:   dialog {
	     label= "Description";
	: list_box {
		   key = "desc_list";
		   width = 70;
		   height = 12;
		   }
       ok_button;
  }

help_lsp:   dialog {
	label= "HELP";
	: list_box {
		   key = "lsp_help";
		   width = 67;
		   height = 12;
		   }
	 ok_button;
     }

lsp_info:   dialog {
	label="INFORMATION";
      :row {
	: image {
		   key = "lsp_view";
		   width = 20;
		   height = 12;
		   alignment = centered;
		   }
	: list_box {
		   key = "infobox";
		   width = 35;
		   height = 12;
		   }
	   }
	 ok_button;
     }

lsp_search:  dialog {
 initial_focus = "search_me";     
	    label="SEARCH";
	:edit_box {
	key = "search_me";
	edit_width = 32;
	edit_limit = 32;
	fixed_width = true;
	allow_accept = false;
	}
      :row { 
	:toggle {
	label="Match whole word";
	key = "match_makr";
	}
	ok_button;
	cancel_button;
	       }
		       }

lsp_config:  dialog {
	label="Super Lisp Library Configuration";          
          :edit_box {
	   label="Lisp Path:";
	   key="lsp_path";
	   edit_width=30;
			}
      :boxed_column {
           label="Lisp Registration";
          :edit_box {
	   label="Lisp Name:";
	   key="lsp_nm";
	   edit_width=30;
			}
           :edit_box {
	   label="Registration #:";
	   key="lsp_reg";
	   edit_width=30;
			}
           }
	:toggle {
	label="Add Lisps to Apploads";
	key = "APLIST_ADD";
             }
       
            ok_only;
                           }
