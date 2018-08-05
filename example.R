
# library(cleanr)
devtools::load_all("/Users/alastairrushworth/Documents/cleanr/cleanr")
data(mtcars, band_instruments, nasa, starwars, storms, airquality)

# TODO
# push up to github
# add table headings and explanations
# pad output more carefully to faciliate the above
# consider other palletes that are more red/green colour friendly
# add number of columns to printed output - where appropriate
# add redundant feature table - cooccurence
# use machine learning to guess what is a date and what isn't



mtcars           %>% report 
band_instruments %>% report
nasa             %>% report
starwars         %>% report
storms           %>% report
airquality       %>% report


# types
mtcars           %>% report_types
band_instruments %>% report_types
nasa             %>% report_types
starwars         %>% report_types
storms           %>% report_types
airquality       %>% report_types

# report NA
mtcars           %>% report_na
band_instruments %>% report_na
nasa             %>% report_na
starwars         %>% report_na
storms           %>% report_na
airquality       %>% report_na

# report cor
mtcars           %>% report_cor
band_instruments %>% report_cor
nasa             %>% report_cor
starwars         %>% report_cor
storms           %>% report_cor
airquality       %>% report_cor

# report space
mtcars           %>% report_space
band_instruments %>% report_space
nasa             %>% report_space
starwars         %>% report_space
storms           %>% report_space
airquality       %>% report_space

