source('https://raw.githubusercontent.com/mosscoder/restackor_opt/main/ro_funs.R')

source('https://raw.githubusercontent.com/mosscoder/restackor_opt/main/ro_funs.R') # restackor opt code

library(tidyverse)
library(patchwork)

results_loc <- 'Z:\\Downloads\\stack_search_first_stage' # folder to save results

load_database_params('charger_2_1_base') # load the charger 2.1 damper properties into ReStackor

set_adjusters(n_click = 9, # set the adjusters
              preload = 2,
              u_wheel = 8)

tg <- expand.grid(
  shim_count = 2:12,
  shim_thickness = 0.1,
  start_width = 18,
  end_width = c(10:18),
  shape =  c(0,0.1, 0.5, 1, 2, 3, 5, 10),
  clamp_width = 8:15,
  clamp_thickness = c(0.2, 0.4),
  nut_width = 10,
  nut_thickness = 10
)

build_stack_from_grid <- function(tune_grid, x){
  focal_params <- tune_grid[x, ]
  if(focal_params$shape != 0){
    base_stack <- make_add_profile(start_shim = focal_params$start_width,
                                   end_shim = focal_params$end_width,
                                   shim_count = focal_params$shim_count,
                                   shape = focal_params$shape
    )
  } else {
    base_stack <- rep(focal_params$start_width, focal_params$shim_count)
  }
  
  if(tail(base_stack, 1) <= focal_params$clamp_width)
    return(NA)
  
  width <- c(base_stack, focal_params$clamp_width, focal_params$nut_width)
  thickness <- c(rep(focal_params$shim_thickness, 
                     focal_params$shim_count),
                 focal_params$clamp_thickness, focal_params$nut_thickness)
  
  return(cbind(width, thickness))
  
}

result <- lapply(FUN = build_stack_from_grid, X = seq_len(nrow(tg)), tune_grid = tg)
keeper_inds <- which(!duplicated(result) & !is.na(result))
keeper_params <- tg[keeper_inds,]
finals <- result[keeper_inds]

write.csv(finals, file.path(results_loc, 'all_params.csv'))

run_across_grid <- function(x, stack, OF = results_loc){
  focal_stack <- stack[[x]] %>% 
    as.data.frame() %>% 
    mutate(combine_dims = paste(width, str_replace_all(thickness, '\\.','o'), sep ='x'))
  
  ind_width <- floor(log10(length(stack))) + 1
  
  stack_nm <- capture.output(cat(focal_stack$combine_dims, sep ='_')) 
  fname <- paste0(str_pad(x, width = ind_width, pad = '0'),
                  '_',
                  stack_nm, 
                  '.csv')
  
  run_shimstack(shim_df = focal_stack[,1:2],
              outfile = file.path(OF, fname))
}

lapply(FUN = run_across_grid, seq_along(finals), OF = results_loc)
