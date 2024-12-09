## code to prepare `aux_info` dataset goes here

# TODO: get these from a query, since they are subject to change
concurrent_list <- read.csv("sandbox/analytics_quad_concurrent_courses.csv")
building_list <- read.csv("sandbox/analytics_quad_buildings.csv")

# ISO country codes from csv
iso_countries <- read.csv("sandbox/iso-countries.csv")
country_codes <- setdiff(iso_countries$iso_alpha2, "")

# CIP codes from csv
cipdf <- read.csv("sandbox/CIPCode2020.csv")
cip_codes <- cipdf %>%
  mutate(cip_chr = sprintf(fmt = "%06g", CIPCode * 1e4)) %>% # Need a 6-digit string with no decimals
  add_row(cip_chr = "999999") # Appending this cip code since all majors including non-degree are assigned a cip code.

# Degree types from csv
degree_types <- read.csv("sandbox/degree-types.csv")

# High schools from text file
highschools <- read.csv("sandbox/highschools.txt", sep = "|")
ut_highschools <- highschools %>%
  filter(HS_State == "UT") %>%
  pull(HS_ACT_Code)

# Campus IDs from file--supplied by Justin
campus_ids <- scan("sandbox/valid_campus_ids.txt", what = character(0))


previous_degree_types <- c("1", "1A", "1B", "2", "3", "4", "5", "6", "7", "8", "17",
                           "18", "19", "DIP", "CER", "AX", "BX", "MX", "DX", "FP")


final_grades <- c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-",
  "E", "F", "UW", "I", "IP", "CR", "NC", "AU", "SP", "P", "W", "L", "NG", "T", "CW")
passing_grades <- c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-",
                    "CR", "SP", "P", "NG", "T")

room_use_codes <- c(
  "100", "110", "115", "200", "210", "215", "220", "225", "230", "235", "255",
  "300", "310", "315", "350", "355", "400", "410", "420", "430", "440", "455", "500",
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


# Auxiliary information, a list of objects that can be referenced by checklist functions
aux_info <- list(

  # Valid values
  valid_s_reg_statuses = s_reg_statuses, #S17a

  valid_c_line_items = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "K", "L",
                         "M", "N", "P", "Q", "R", "S", "T", "X"), #C09

  valid_ushe_instruction_method_codes = c("P", "H", "T", "R", "I", "B", "C", "O", "V", "Y"), #C12

  valid_instruction_method_codes = c("P", "H", "T", "R", "I", "B", "C", "O", "V", "Y", "E"), #UTC01

  valid_program_types = c("A", "V", "P", "C"), # C13

  valid_instruct_types = c("LEC", "LEL", "LAB", "SUP", "INV", "THE", "DIS", "CON", "OTH"), #C44a

  valid_cip_codes = cip_codes$cip_chr, #G09a

  valid_country_codes = country_codes, #S27c

  valid_highschools = unique(highschools$HS_ACT_Code), #C48a

  non_concurrent_highschools = c("459050","459100","459150","459200","459300",
                                 "459400","459500","459000"), #SC12b

  concurrent_course_ids = concurrent_list$course_id, #C11b

  ut_highschools = ut_highschools,

  valid_degree_ids = degree_types$degree_type, #S19a

  valid_previous_degree_types = previous_degree_types, #G16a

  valid_level_class_ids = s_levels, #S18a

  valid_seasons = c("Summer", "Fall", "Spring"), #G25a

  valid_final_grades = final_grades, #SC10a

  passing_grades = passing_grades, #SC08d

  valid_building_location_codes = c("MC", "RP", "BC", "IC", "OIS", "DEC", "SPC", "SPS"), #B02b

  valid_ownership_codes = c("O", "V", "R", "F", "H", "L"), #B03b

  valid_building_condition_codes = as.character(1:5), #B11b

  valid_room_group1_codes = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
                              "K", "L", "M", "N", "O", "S", "V", "X", "Y", "Z"), #R06b

  valid_room_use_code_groups = c(as.character(0:9 * 100),
                                "000", "NON", "WWW", "XXX", "YYY", "ZZZ"), #R07b

  valid_room_use_codes = c(room_use_codes), #R08b

  valid_student_type_codes = c('N','2','R','C','T','3','P','H','0','5','1','F','S'),

  valid_campus_ids = campus_ids,

  valid_ipeds_degree_award_levels = c("2", "7", "3", "1B", "1A", "5"), #S19 & G17

  # Inventories
  building_inventory = building_list$building_number, #C19c and others, TODO: get from a query
  rooms_inventory = c(building_list$building_number) #C19d and others, TODO: get from a query
)


usethis::use_data(aux_info, overwrite = TRUE)
