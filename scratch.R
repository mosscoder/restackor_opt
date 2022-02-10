source('https://raw.githubusercontent.com/mosscoder/restackor_opt/main/ro_funs.R')

stack_1 <- c(18, 17, 16, 15, 14, 13, 12, 11, 10)
stack_2 <- c(18, 18, 18, 18, 16, 14, 12, 11, 10)
stack_3 <- c(18, 15, 13, 11, 10, 10, 10, 10, 10)

examps <- data.frame(stack_1, stack_2, stack_3) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(stack_1:stack_3, names_to = 'stack', values_to = 'width')

ggplot(examps, aes(x = factor(id), y = width)) +
  geom_bar(stat = 'identity') +
  theme_classic() +
  facet_wrap(~stack)

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
finals[[1]]
