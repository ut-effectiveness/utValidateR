## code to prepare `aux_info` dataset goes here


# CIP codes from csv
cipdf <- read.csv("sandbox/CIPCode2020.csv")

cip_codes <- cipdf %>%
  mutate(cip_chr = sprintf(fmt = "%06g", CIPCode * 1e4)) # Need a 6-digit string with no decimals

room_use_codes <- c(
  "100", "110", "115", "200", "210", "215", "220", "225", "230", "235", "255",
  "310", "315", "350", "355", "400", "410", "420", "430", "440", "455", "500",
  "510", "515", "520", "523", "525", "530", "535", "540", "545", "550", "555",
  "560", "570", "575", "580", "585", "590", "600", "610", "615", "620", "625",
  "630", "635", "640", "645", "650", "655", "660", "665", "670", "675", "680",
  "685", "700", "710", "715", "720", "725", "730", "735", "740", "745", "750",
  "755", "760", "765", "770", "775", "780", "800", "810", "815", "820", "830",
  "835", "840", "845", "850", "855", "860", "865", "870", "880", "890", "895",
  "900", "910", "919", "920", "935", "950", "955", "970", "0", "50", "60", "70",
  "NON", "WWW", "W01", "W02", "W03", "W04", "W05", "W06", "W07", "XXX", "X01",
  "X02", "X03", "X04", "X05", "YYY", "Y01", "Y02", "Y03", "Y04", "ZZZ")


s_levels <- c("FR", "SO", "JR", "SR", "UG", "GG", "GN") # AKA primary_level_class_id
s_reg_statuses <- c("HS", "FH", "FF", "TU", "NG", "TG", "CS", "RS", "CG",
                    "RG", "NM", "CE", "NC")
undergrad_student_type_codes <- c('P', 'T', 'R', 'C', 'F', 'H')
grad_student_type_codes <- c('1', '5', '2', '4')

aux_info <- list(

  # Valid values
  valid_s_reg_statuses = s_reg_statuses, #S17a

  valid_c_line_items = c("a","b","c","d","e","f","g","h","i","p","q","r","s","t","x"), #C09

  valid_instruction_method_codes = c("P", "H", "T", "R", "I", "B", "C", "O", "V", "Y"), #C12

  valid_program_types = c("A", "V", "P", "C"), # C13

  valid_section_format_type_codes = c(), #C44a

  valid_cip_codes = cip_codes$cip_chr, #G09a

  valid_level_class_ids = s_levels, #S18a

  valid_seasons = c("1", "2", "3"), #G25a

  valid_final_grades = c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-",
                         "E", "F", "UW", "I", "IP", "CR", "NC", "AU", "SP", "P", "W",
                         "L", "NG", "T", "CW"), #SC10a

  valid_building_location_codes = c("MC", "RP", "BC", "IC", "OIS", "DEC", "SPC", "SPS"), #B02b

  valid_ownership_codes = c("O", "V", "R", "F", "H", "L"), #B03b

  valid_building_condition_codes = as.character(1:5), #B11b

  valid_room_group1_codes = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
                              "K", "L", "M", "N", "O", "S", "V", "X", "Y", "Z"), #R06b

  valid_room_use_code_groups = c(as.character(0:9 * 100),
                                 "NON", "WWW", "XXX", "YYY", "ZZZ"), #R07b

  valid_room_use_codes = c(room_use_codes), #R08b

  # Inventories
  building_inventory = c(), #C19c and others

  rooms_inventory = c() #C19d and others
)

usethis::use_data(aux_info, overwrite = TRUE)
