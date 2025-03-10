library(XLConnect)
library(tidyverse)

restackor_xls_loc <- 'C:\\ReStackor\\Excel\\metric-ReStackor.xls'
restackor_csv_loc <- 'C:\\ReStackor\\Work_Dir\\restackor.csv'
executable <- 'C:\\ReStackor\\Code\\restackor.exe'
results_loc <- 'Z:\\Downloads\\restackor_results'

if(!dir.exists(results_loc)){dir.create(results_loc)}

#### Set static damper properties ####

#These are mostly estimates for Rockshox Charger 2.1

damper_properties <- # format: property value, excel row, excel column
  list(shim_id = c(1, 6, 3), # ID shims
       d_rod = c(10, 4, 8), # damper rod diameter
       d_valve = c(18, 4, 9), # damper body ID (estimate)
       w_seat = c(1, 4, 10),  # port seat width (estimate)
       v_spec = c('BVc', 4, 11), # stroke direction (base valve)
       r_port = c(2, 6, 8), # The radial distance from the valve center to the inside edge of the valve port in millimeters
       d_port = c(3,6,9), #radial length of port
       w_port = c(4,6,10 )
       
       
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
shim_widths <- c(10,
                 10,
                 5,
                 9,
                 7,
                 5,
                 4,
                 10
                 )

shim_thickness <- c(0.05,
                    0.05,
                    0.03,
                    0.075,
                    0.075,
                    0.075,
                    0.15,
                    0.4
                    )

shims <- data.frame(width = shim_widths,
                    thickness = shim_thickness
)

res_path <- file.path(results_loc, 'my_stack.csv')

run_shimstack(shim_df = shims,
              outfile = res_path)

#### Plot outcomes ####
result <- read.csv(res_path, skip = 1)[-1,] %>% 
  sapply(., as.numeric ) %>% 
  as.data.frame()

colnames(dat) <- str_remove_all(colnames(result), 'X.')

ggplot(
  dat[1:18, ] %>%
    pivot_longer(
      cols = U.clk:U.clsd,
      names_to = 'setting',
      values_to = 'velocity'
    ),
  aes(
    y = Fstack,
    x = velocity,
    group = setting,
    color = setting
  )
) +
  geom_line(size = 1.5, alpha = 0.6) +
  scale_color_manual(
    labels = c('Middle', 'Closed', 'Open'),
    name = 'Clicker\nsetting',
    values = c('black', 'red', 'blue')
  ) +
  xlab('Shaft velocity (m/sec)') +
  ylab('Force (kgf)')
