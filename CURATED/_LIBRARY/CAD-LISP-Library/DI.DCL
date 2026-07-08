gaze:   dialog {
        label= "DIMENSIONAL DISTANCE CALCULATOR";
        width=30;
        :row {
        : image {
                   key = "view";
                   width = 20;
                   height = 12;
                   alignment = centered;
                   }

   :boxed_column {
    width = 1;
    height = default;
    layout = vertical;
    label = "Select Dimensions To Be Toggled OFF";
    boxed = true;
    :toggle {
        width = 5; 
        label = "X";
        mnemonic = "X";
        key = "X";
                }

    :toggle {
        width = 5;
        label = "Y";
        mnemonic = "Y";
        key = "Y";
                }

    :toggle {
        width = 5; 
        label = "Z";
        mnemonic = "Z";
        key = "Z";
                }
}
}
         :row {
          alignment = centered;
          fixed_width = true;
           :button {
                   is_default=true;
                   label = "PICK";
                   key = "accept";
                   mnemonic = "P";
                  }
          :spacer {width = 2;}
         cancel_button;
         errtile;
     
        }
  }
