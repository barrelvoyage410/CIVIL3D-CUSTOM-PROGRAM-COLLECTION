gaze:   dialog {
        label= "ASCII TEXT VIEWER";
        :row {
        : image {
                   key = "view";
                   width = 20;
                   height = 12;
                   alignment = LEFT;
                   }
        
        : list_box {
                   key = "gazebox";
                   width = 50;
                   height = 12;
                   }

                   }
         :row {
          alignment = centered;
          fixed_width = true;
           :button {
                   label = "OPEN";
                   key = "accept";
                   mnemonic = "O";
                  }
          :spacer {width = 2;}
          :button {
                  label="PRINT";
                  key= "print";
                  mnemonic="P";
                  }
          :spacer {width = 2;}
         cancel_button;
     
        }
  }

port_config:  dialog {
	label="Printer Port Configuration";          
          :edit_box {
	   label="Printer Port:";
	   key="pport";
	   edit_width=6;
			}
 ok_only;
}
