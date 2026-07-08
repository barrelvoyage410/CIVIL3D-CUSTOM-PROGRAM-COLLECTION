art:   dialog {
        label= "ART - ARced Text creator";
 :row {
        //label="         By Ken Melvin 1996";
          label="Orientation";
           :radio_button {
            fixed_width = true;
            fixed_height = true;
            label = "Inner";
            mnemonic = "I";
            key = "in";
                }
           :radio_button {
            fixed_width = true;
            fixed_height = true;
            label = "Outer";
            mnemonic = "O";
            key = "out";
                }
          :edit_box {
           label="Text Height: ";
           mnemonic= "H";
           key="hte";
           edit_width=4;
           fixed_width=true;
           alignment=left;
                        }


                }
 :row {      
        :boxed_column {
        label="Radius Point:";
       // :boxed_column {
            :button {
             label = "Pick Pt. <";
             mnemonic = "S";
             key = "Sel_Rad";
                            }
          :edit_box {
           label="X:";
           mnemonic= "X";
           key="xr";
           edit_width=5;
           fixed_width=true;
           alignment=left;

                        }
          :edit_box {
           label="Y:";
           mnemonic= "Y";
           key="yr";
           edit_width=5;
           fixed_width=true;
           alignment=left;

                        }
          :edit_box {
           label="Z:";
           mnemonic= "Z";
           key="zr";
           edit_width=5;
           fixed_width=true;
           alignment=left;

                        }
                        }
        :boxed_column {
        label="Text Location:";
       // :boxed_column {
            :button {
             label = "Pick Pt. <";
             mnemonic = "S";
             key = "Sel_Loc";
                            }
          :edit_box {
           label="X:";
           mnemonic= "X";
           key="xl";
           edit_width=5;
           fixed_width=true;
           alignment=left;

                        }
          :edit_box {
           label="Y:";
           mnemonic= "Y";
           key="yl";
           edit_width=5;
           fixed_width=true;
           alignment=left;

                        }
          :edit_box {
           label="Z:";
           mnemonic= "Z";
           key="zl";
           edit_width=5;
           fixed_width=true;
           alignment=left;

                        }
                   :concatenation {
                    : text_part {
                    label = "Radius= ";
                    }
                    : text_part {
                    key = "radius";
                    width=5;
                            }
                            }
                            }
   }
          :edit_box {
           label="TEXT:";
           mnemonic= "T";
           key="text";
           edit_width=40;
                        }

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
                   label = "DRAW";
                   key = "draw";
                   mnemonic = "D";
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
                   width = 60;
                   height = 12;
                   }
         ok_button;
     }




