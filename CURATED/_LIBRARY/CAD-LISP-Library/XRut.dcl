gaze:   dialog {
        label= "LAYER UTILITIES";
        :row {
        : image {
                   key = "view";
                   width = 20;
                   height = 12;
                   alignment = centered;
                   }
        : list_box {
                   key = "gazebox";
                   width = 40;
                   height = 12;
                   }
                   }
         :row {
          alignment = centered;
           :button {
                   fixed_width=true;
                   label = "INFO..";
                   key = "info";
                   mnemonic = "I";
                  }
          :spacer {width = 2;}
         ok_button;
     
        }

        
  }

info:   dialog {
        label= "INFORMATION";
        : list_box {
                   key = "gazer";
                   width = 40;
                   height = 12;
                   }
         ok_button;
     }


