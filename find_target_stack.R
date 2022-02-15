source('https://raw.githubusercontent.com/mosscoder/restackor_opt/main/ro_funs.R') # restackor opt code

library(tidyverse)
library(patchwork)
library(mgcv)

padz <- function(x){str_pad(x, width = 2, pad = '0')}

find_target_stack <- function(stack_dbase,
                              targ_points, 
                              top_n = 3, 
                              vlim = 6, 
                              dbase_garbage = NULL){
  
  if(nrow(targ_points) < 2) return(print('Too few point targets. Enter a minimum of two.'))
  
  dbase_files <- list.files(stack_dbase, full.names = T)
  garbage_ind <- which(str_detect(dbase_files, dbase_garbage))
  dbase_files <- dbase_files[-garbage_ind]
  
  colnames(targ_points) <- c('U.clk', 'Fshaft')
  targ_points <- rbind(data.frame(U.clk = 0, Fshaft = 0), targ_points)
  targ_mod <- gam(Fshaft ~ s(U.clk, bs="cr", k = nrow(targ_points)), data = targ_points)
  
  candidate_error <- parallel::mclapply(
    FUN = function(x) {
      candidate <- tidy_restackor_results(dbase_files[x]) %>%
        filter(U.clk <= vlim)
      target_force <- predict(targ_mod, newdata = candidate %>% select(U.clk))
      data.frame(stack = dbase_files[x],
                 mape = MLmetrics::MAPE(y_true = target_force, y_pred = candidate$Fshaft)
      )
      
    },
    X = seq_along(dbase_files),
    mc.cores = 6L
  )
  
  ranked_stacks <- do.call(rbind, candidate_error) %>% 
    arrange(mape)
  
  top <- do.call(rbind, lapply(FUN = \(x)tidy_restackor_results(ranked_stacks$stack[x]) %>%
                                 filter(U.clk <= 6.5) %>% 
                                 mutate(Stack = x,
                                        mape = ranked_stacks$mape[x]) %>% 
                                 select(Fshaft, U.clk, Stack, mape) ,
                               X = seq_len(top_n)
  )
  ) %>% 
    mutate(Stack = factor(Stack))
  
  p1 <- ggplot(top, aes(x = U.clk, y = Fshaft, color = Stack, group = Stack)) +
    geom_line(size = 1, alpha = 0.8) +
    geom_point(data= targ_points, aes(x = U.clk, y = Fshaft),
               color = 'red', alpha = 0.75, size = 3.5, inherit.aes = F) +
    theme_bw() +
    xlab('Shaft velocity (m/s)') +
    ylab('Force (kgf)') +
    theme(legend.position = 'top')
  
  b_stack_dat <-
    do.call(rbind, lapply(
      FUN = \(x) stack_from_file(ranked_stacks$stack[x]) %>%
        mutate(Stack = x,
               mape = ranked_stacks$mape[x]),
      X = seq_len(top_n)
    ))
  
  p2 <- ggplot(b_stack_dat %>% 
           group_by(Stack) %>% 
           mutate(n = row_number()) %>% 
           filter(n != max(n)), aes(y = width/2, x = factor(n), width = thickness*1.5)) +
    geom_bar(stat = 'identity', fill = 'black') +
    geom_label(aes(x = factor(n), y = 1, label = paste(width, thickness, sep = 'x')),
               size = 1.5) +
    facet_wrap(~ paste0('Stack ', Stack, ' MAPE: ', round(mape,3)*100, '%'), ncol = 1) +
    scale_y_continuous(breaks = seq(0, round(max(b_stack_dat$width/2)))) +
    coord_flip() +
    theme_classic() +
    xlab('') +
    ylab('Shim radius (mm)')
  
  print(p1 + p2 + plot_layout(width = c(2,1)))
  
  return(ranked_stacks %>% head(top_n))
}

targ_v <- c(1.25, 6)
targe_f <-  c(2, 20)
target_points <- data.frame(velocity = targ_v,
                            kgf = targe_f)

best_stacks <- find_target_stack(stack_dbase = '~/restackor_opt/two_stage_search/',
                                   dbase_garbage = 'all_params.csv',
                                   targ_points = target_points)

targ_v <- c(1.25, 6)
targe_f <-  c(2, 25)
target_points <- data.frame(velocity = targ_v,
                            kgf = targe_f)

best_stacks <- find_target_stack(stack_dbase = '~/restackor_opt/two_stage_search/',
                                 dbase_garbage = 'all_params.csv',
                                 targ_points = target_points)
