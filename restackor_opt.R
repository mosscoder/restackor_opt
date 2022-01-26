library(XLConnect)
library(tidyverse)

restackor_xls_loc <- 'C:\\ReStackor\\Excel\\metric-ReStackor.xls'
restackor_csv_loc <- 'C:\\ReStackor\\Work_Dir\\restackor.csv'
executable <- 'C:\\ReStackor\\Code\\restackor.exe'
results_loc <- 'Z:\\Downloads\\restackor_results'

if(!dir.exists(results_loc)){dir.create(results_loc)}

wb <- loadWorkbook(restackor_xls_loc)

#### Set static damper properties ####
damper_properties <- 
  list(shim_id = c(6, 6, 3), #property value, excel row, excel column
       d_rod = c(2.5, 4, 8)
       )

for(i in names(damper_properties)){
  focal_prop <- damper_properties[[i]]
  writeWorksheet(
    object = wb,
    data = focal_prop[1],
    startRow = focal_prop[2],
    startCol = focal_prop[3],
    sheet = 'Plots',
    header = F
  )
}

saveWorkbook(wb)

#### Function to execute restackor ####

run_shimstack <- function(shim_df, outfile) {
  wb <- loadWorkbook(restackor_xls_loc)
  
  clearRange(object = wb,
             sheet = 'Plots',
             coords = c(9, 3, 58, 4))
  
  writeWorksheet(
    object = wb,
    data = shims,
    startRow = 9,
    startCol = 3,
    sheet = 'Plots',
    header = F
  )
  
  saveWorkbook(wb)
  
  system(executable)
  
  file.copy(from = restackor_csv_loc,
            to = outfile)
}

#define shimstack
shim_widths <- c(10, 10, 10, 7, 8)
shim_thickness <- c(0.1, 0.1, 0.1, 0.2, 2)

shims <- data.frame(width = shim_widths,
                    thickness = shim_thickness
)

run_shimstack(shim_df = shims,
              outfile = file.path(results_loc, 'my_stack.csv'))
