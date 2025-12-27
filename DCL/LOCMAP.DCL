LOCMAP : dialog {
label = "Location Map Creator";
: column {
	: edit_box {
		key = "LATITUDE";
		label = "Latitude (e.g. 43.934358)";
		edit_width = 20;
		value = "";
	}
	: edit_box {
		key = "LONGITUDE";
		label = "Longitude (e.g. -70.345937)";
		edit_width = 20;
		value = "";
	}
		: edit_box {
		key = "LATLONG";
		label = "Latitude,Longitude (e.g. 44.099069, -70.215315)";
		edit_width = 20;
		value = "";
	}	
	: edit_box {
		key = "SCALE";
		label = "Property Map Scale";
		edit_width = 20;
		value = "100";
	}
	
	
: row {
	: button {
		key = "create-loc";
		label = " create location ";
		is_default = false;
	}
	: button {
		key = "create-prop";
		label = " create property";
		is_default = false;
	}
	: button {
		key = "create-both";
		label = " create both";
		is_default = false;
	}	
	}
	
	
	
	
	
	: row {

	: button {
		key = "insert-loc";
		label = " insert location ";
		is_default = false;
	}
	: button {
		key = "insert-prop";
		label = " insert property ";
		is_default = false;
	}
	: button {
		key = "insert-both";
		label = " insert both ";
		is_default = false;
	}	
	}
	
	
	
	: button {
		key = "cancel";
		label = " Cancel - End";
		is_default = true;
		is_cancel = true;
	}

}
}