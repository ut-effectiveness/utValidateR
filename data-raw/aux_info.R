## code to prepare `aux_info` dataset goes here

aux_info <- list(

  # Valid values
  valid_s_reg_statuses = c(), #S17a
  valid_c_line_items = c("a","b","c","d","e","f","g","h","i","p","q","r","s","t","x"), #C09
  valid_instruction_method_codes = c(), #C12
  valid_program_types = c(), # C13
  valid_section_format_type_codes = c(), #C44a
  valid_cip_codes = c(), #G09a
  valid_seasons = c("1", "2", "3"), #G25a
  valid_final_grades = c(), #SC10a
  valid_building_location_codes = c(), #B02b
  valid_ownership_codes = c(), #B03b
  valid_building_condition_codes = c(), #B11b
  valid_room_group1_codes = c(), #R06b
  valid_room_use_code_groups = c(), #R07b
  valid_room_use_codes = c(), #R08b

  # Inventories
  building_inventory = c(), #C19c and others
  rooms_inventory = c() #C19d and others
)

usethis::use_data(aux_info, overwrite = TRUE)
