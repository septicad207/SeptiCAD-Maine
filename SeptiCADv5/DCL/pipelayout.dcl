PIPELAYOUT : dialog {

  label = "  Distribution Pipe Layout  ";

     : boxed_column {
 
              : popup_list {
                key = "TYPE";
                label = "     Type of Distribution Layout: ";
                width = 10;
                value = "0";
		action = "(checkDistributionPipeDialog)";
              }

              : popup_list {
                key = "SIDE";
                label = "     Side of Distribution: ";
                width = 10;
                value = "0";
		action = "(checkDistributionPipeDialog)";
              }

	      : edit_box {
                key = "SEQ";
                label = "     Row Sequence (see below): ";
                edit_width = 20;
		action = ""; 
                value = "";
              }


				 : image { 
                                   alignment = centered;	
				   key = "IMAGE"; 
				   fixed_height = true;
				   fixed_width = true;
				   height = 20; 
				   width =  70;  
				   color = 5;
				 } 


             : row {
	        : button {
	          key = "okay";
	          label = "  Okay  ";
	          is_default = true;
	          is_cancel = false;
	        }

	        : button {
	          key = "ignore";
	          label = "  Ignore/Skip ";
	          is_default = false;
	          is_cancel = true;
	        }
              }
   }

}