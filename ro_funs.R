restack_in <- 'C:\\ReStackor\\Work_Dir\\Stack.in'
restack_csv <- 'C:\\ReStackor\\Work_Dir\\restackor.csv'
restackor_exec <- 'C:\\ReStackor\\Code\\restackor.exe'

estimate_coil_rate <- function(d_wire, d_coil, n_active){
  top <- 8080*(d_wire^4)
  bottom <- (8*(d_coil - d_wire)^3)*n_active
  top/bottom
}

change_line_ind <- function(line, ind, val){
  stack <- readLines(restack_in)
  focal_line <- stack[line]
  line_broken <- str_split(focal_line, pattern = ',')
  line_broken[[1]][ind] <- val
  writeLines(str_flatten(line_broken[[1]], collapse = ','), restack_in)
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
  
  param_map <- 
    list(si = c(11, 3, shim_id), 
         f = c(11, 4, float), 
         dr = c(2, 2, d_rod), 
         dv = c(2, 3, d_valve),  
         ws = c(2, 4, w_seat),
         vs = c(2, 5, v_spec),
         rp = c(4, 1, r_port), 
         dp = c(4, 2, d_port), 
         wp = c(4, 3, w_port),
         np = c(4, 4, n_port),
         hd = c(4, 5, h_deck),
         dl = c(4, 6, d_leak),
         dt = c(4, 7, d_throat),
         nt = c(4, 8, n_throat),
         db = c(4, 9, d_bleed),
         mc = c(4, 10, max_click),
         dh = c(7, 1, d_hsc),
         k = c(7, 3, k_spring)
    )
  
  for (i in names(param_map)) {
    focal_prop <- param_map[[i]]
    
    if (!is.null(focal_prop)) {
      change_line_ind(line = focal_prop[1],
                      ind = focal_prop[2],
                      val = focal_prop[3])
    }
  }
  
}

load_database_params <- function(damper_name){
  dloc <- tempfile()
  download.file('https://docs.google.com/spreadsheets/d/e/2PACX-1vQ6d6Dh3Yz9j4ANKedh1fBz0g-LsnqcaASWh3VeZQqyVIrVj0EJDV8DRyAMbqtg0DpE8Ndlde3hi4Rf/pub?output=csv',
                destfile = dloc)
  
  focal_props <- read.csv(dloc)[, c('param', damper_name)]
  flist <- focal_props[,2, drop = TRUE] 
  names(flist) <- focal_props[,1]
  
  set_fixed_params(shim_id =  flist['shim_id'] %>% as.numeric,
                   float =  flist['float'] %>% as.numeric,
                   d_rod  =  flist['d_rod'] %>% as.numeric,
                   d_valve =  flist['d_valve'] %>% as.numeric,
                   w_seat =  flist['w_seat'] %>% as.numeric,
                   v_spec =  flist['v_spec'] %>% as.character,
                   r_port =  flist['r_port'] %>% as.numeric,
                   d_port =  flist['d_port'] %>% as.numeric,
                   w_port =  flist['w_port'] %>% as.numeric,
                   n_port =  flist['n_port'] %>% as.numeric,
                   h_deck =  flist['h_deck'] %>% as.numeric,
                   d_leak =  flist['d_leak'] %>% as.numeric,
                   d_throat =  flist['d_throat'] %>% as.numeric,
                   n_throat =  flist['n_throat'] %>% as.numeric,
                   d_bleed =  flist['d_bleed'] %>% as.numeric,
                   max_click =  flist['max_click'] %>% as.numeric,
                   d_hsc =  flist['d_hsc'] %>% as.numeric,
                   k_spring =  flist['k_spring'] %>% as.numeric
                   )
}

set_adjusters <- function(n_click = NULL,
                          f_max = NULL,
                          u_wheel = 8,
                          preload = NULL) {
  adjust_map <-
    list(
      nc = c(9, 1, n_click),
      fm = c(11, 1, f_max),
      uw = c(11, 2, u_wheel),
      p = c(7, 2, preload)
    )
  
  
  for (i in names(adjust_map)) {
    focal_prop <- adjust_map[[i]]
    
    if (!is.null(focal_prop[3])) {
      change_line_ind(line = focal_prop[1],
                      ind = focal_prop[2],
                      val = focal_prop[3])
    }
  }
  
}
                    
run_shimstack <- function(shim_df, outfile) {
  stack <- readLines(restack_in)
  for(i in 13:62){
    stack[i] <- paste(i-12,-1,-1, sep = ',')
  }
  
  for(i in seq_len(nrow(shim_df))){
    stack[i+12] <- paste(i,shim_df[i,1],shim_df[i,2], sep = ',')
  }
  
  
  writeLines(stack, restack_in)
  
  system(restackor_exec)
  
  file.copy(from = restack_csv,
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
