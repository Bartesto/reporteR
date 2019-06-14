path <- "../nutrient_monitoring/swan_nutrient_WIN"

files <- list.files(path = path, pattern = ".xlsx",
                    full.names = TRUE)

temp_names <- names(readxl::read_excel(files[2]))
temp_names2 <- names(readxl::read_excel(files[2], skip = 1))
new_names <- c(temp_names2[1:24], temp_names[25:length(temp_names)])

d1 <- readxl::read_excel(files[2], skip = 2, col_names = new_names, .name_repair = ~ janitor::make_clean_names)

