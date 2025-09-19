NW_CLP_all_points.csv was copied from the NW-CLP-RS repository ("a_locs_poly_setup/out")
- Waterbodies that **might** be visible, use `filter(data_group == “CLP”)` to 
get the CLP waterbodies only

clp_sdd_rs_estimate_v2024-10-10.feather 
-	SDD estimates: exported from the regional-clarity-rs-model repo (script 5, 
currently only in b's fork). use the mean value here (reported in meters). 
`rowid` in this matches `rowid` in waterbodies .csv above (and should also 
match permanent_id and gnis_name, if they don’t, let me know!)

clp_temp_rs_estimate_nocorr_v2024-10-10.feather
-	Temp estimates: exported from the regional-clarity-rs-model repo (script 5,
currently only in b's fork) – these are in Kelvin, please display in C and F. 
this version does not have the linear bias correction applied. like sdd file 
above, `rowid` in this matches `rowid` in waterbodies .csv above
