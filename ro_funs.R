restackor_xls <- 'C:\\ReStackor\\Excel\\metric-ReStackor.xls'
restack_csv <- 'C:\\ReStackor\\Work_Dir\\restackor.csv'
restackor_exec <- 'C:\\ReStackor\\Code\\restackor.exe'

estimate_coil_rate <- function(d_wire, d_coil, n_active){
  top <- 8080*(d_wire^4)
  bottom <- (8*(d_coil - d_wire)^3)*n_active
  top/bottom
}

set_fixed_params <- function(shim_id = NULL,
                       float = NULL,
                       d_rod = NULL,
                       d_valve = NULL,
                       w_seat = NULL,
                       v_spec = NULL,
                       r_port = NULL,
                       d_port = NULL,
                       w_port = NULL,
                       n_port = NULL,
                       h_deck = NULL,
                       d_leak = NULL,
                       d_throat = NULL,
                       n_throat = NULL,
                       d_bleed = NULL,
                       max_click = NULL,
                       d_hsc = NULL,
                       k_spring = NULL
                       ) {
  
  wb <- XLConnect::loadWorkbook(restackor_xls_loc)
  
  excel_map <- 
    list(si = c(shim_id, 6, 3), 
         f = c(float, 6, 4), 
         dr = c(d_rod, 4, 8), 
         dv = c(d_valve, 4, 9),  
         ws = c(w_seat, 4, 10),
         vs = c(v_spec, 4, 11),
         rp = c(r_port, 6, 8), 
         dp = c(d_port, 6, 9), 
         wp = c(w_port, 6, 10 ),
         np = c(n_port, 6, 11),
         hd = c(h_deck, 8, 8),
         dl = c(d_leak, 8, 9),
         dt = c(d_throat, 8, 10),
         nt = c(n_throat, 8, 11),
         db = c(d_bleed, 4, 14),
         mc = c(max_click, 4, 15),
         dh = c(d_hsc, 6, 15),
         k = c(k_spring, 6, 16)
         )
  
  for(i in names(excel_map)){
    focal_prop <- excel_map[[i]]
    
    if(!is.null(focal_prop[1])) { 
      
    XLConnect::writeWorksheet(
      object = wb,
      data = focal_prop[1],
      startRow = focal_prop[2],
      startCol = focal_prop[3],
      sheet = 'Plots',
      header = F
    )
      
      }
  }
  
  XLConnect::saveWorkbook(wb)
  
}

load_database_params <- function(damper_name){
  dloc <- tempfile()
  download.file('https://docs.google.com/spreadsheets/d/e/2PACX-1vQ6d6Dh3Yz9j4ANKedh1fBz0g-LsnqcaASWh3VeZQqyVIrVj0EJDV8DRyAMbqtg0DpE8Ndlde3hi4Rf/pub?output=csv',
                destfile = dloc)
  
  focal_props <- read.csv(dloc)[, damper_name]
  set_fixed_params(focal_props)
}

set_adjusters <- function(n_click = NULL,
                          f_max = NULL,
                          u_wheel = 8,
                          preload = NULL) {
  wb <- XLConnect::loadWorkbook(restackor_xls_loc)
  
  excel_map <-
    list(
      nc = c(n_click, 11, 8),
      fm = c(f_max, 11, 9),
      uw = c(u_wheel, 11, 10),
      p = c(pre, 6, 14)
    )
  
  for (i in names(excel_map)) {
    focal_prop <- excel_map[[i]]
    
    if (!is.null(focal_prop[1])) {
      XLConnect::writeWorksheet(
        object = wb,
        data = focal_prop[1],
        startRow = focal_prop[2],
        startCol = focal_prop[3],
        sheet = 'Plots',
        header = F
      )
      
    }
  }
  
  XLConnect::saveWorkbook(wb)
  
}
                    
run_shimstack <- function(shim_df, outfile) {
  wb <- XLConnect::loadWorkbook(restackor_xls)
  
  XLConnect::clearRange(object = wb,
             sheet = 'Plots',
             coords = c(9, 3, 58, 4))
  
  XLConnect::writeWorksheet(
    object = wb,
    data = shim_df,
    startRow = 9,
    startCol = 3,
    sheet = 'Plots',
    header = F
  )
  
  XLConnect::saveWorkbook(wb)
  
  system(restackor_exec)
  
  file.copy(from = restackor_exec,
            to = outfile)
}

tidy_restackor_results <- function(results_file){
  result <- read.csv(results_file, skip = 1)[-1,] %>% 
    sapply(., as.numeric ) %>% 
    as.data.frame()
  
  colnames(result) <- str_remove_all(colnames(result), 'X.')
  return(result)
}

make_add_profile <- function(start_shim, end_shim, shim_count, shape){
  raw_widths <- dweibull(seq_len(shim_count), shape = shape, scale = 1, log = TRUE)
  round(scales::rescale(raw_widths, to = c(end_shim, start_shim)))
}
