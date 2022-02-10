source('https://raw.githubusercontent.com/mosscoder/restackor_opt/main/ro_funs.R') # restackor opt code

library(tidyverse)
library(patchwork)

results_loc <- '~/restackor_opt/first_stage_search'

all_fs <- list.files(results_loc, full.names = T)
result_fs <- all_fs[which(!all_fs %in%  'all_params.csv')]

stack_stacks <- function(x) {
  stock_result <- tidy_restackor_results(result_fs[x])
  if(is.null(stock_result$U.clk))
    return()
  
  stock_result %>% 
    select(U.clk, U.wo, U.clsd, Fshaft) %>% 
    mutate(Stack = x)
}

params <- read.csv(file.path(results_loc, 'all_params.csv'))[,-1] %>% 
  mutate(Stack = factor(row_number()))

results <- do.call(rbind, lapply(FUN = stack_stacks, X = seq_along(result_fs))) %>% 
  mutate(Stack = factor(Stack))

results_with_params <- results %>% left_join(params)

fig_loc <- '~/restackor_opt/figs'
dir.create(fig_loc)

png(file.path(fig_loc, 'all_stacks.png'), height = 6, width = 8, res = 200, units = 'in')
ggplot(
  results %>% group_by(Stack) %>% filter(max(Fshaft) < 300),
  aes(y = U.clk, x = Fshaft, group = Stack, fill = Stack, color = Stack)
) +
  geom_line(size = 0.25, alpha = 0.2) +
  coord_flip(ylim =  c(0.5,10.5))+
  scale_y_continuous(breaks = seq(0, 11, by = 1)) +
  guides(color = 'none', fill = 'none') +
  ylab('Shaft velocity (m/sec)') +
  xlab('Force (kgf)') +
  theme_bw() +
  theme(legend.position = 'top') +
  ggtitle("Produced with www.shimrestackor.com models 
          and libraries from https://github.com/mosscoder/restackor_opt/")
dev.off()

png(file.path(fig_loc, 'clamp_effect.png'), height = 6, width = 9, res = 200, units = 'in')
ggplot(
  results_with_params %>% group_by(Stack) %>% filter(max(Fshaft) < 300),
  aes(y = U.clk, x = Fshaft, group = Stack, color = shim_count)
) +
  geom_line(size = 1, alpha = 0.3) +
  coord_flip(ylim =  c(0.5,10.5))+
  scale_y_continuous(breaks = seq(0, 11, by = 1)) +
  scale_color_viridis_c(name = 'Shim count') +
  ylab('Shaft velocity (m/sec)') +
  xlab('Force (kgf)') +
  theme_bw() +
  facet_wrap(~ paste('Clamp width = ', str_pad(clamp_width, width = 2, pad = '0'), ' mm'), scales = 'free_x') +
  ggtitle("Produced with www.shimrestackor.com models 
          and libraries from https://github.com/mosscoder/restackor_opt/")
dev.off()


make_dweibull <- function(s){
  x <- seq_len(100)
  y <- dweibull(x, shape = s, scale = 1, log = TRUE) %>% scales::rescale()
  data.frame(x, y, s)
}
shapes <- c(0.1,0.5,1.0,2.0,3.0,5.0,10.0)
weibs <- do.call(rbind, lapply(FUN = make_dweibull, 
                      X = shapes))

weib_plot <- ggplot(weibs, aes(x = x, y = y, color = s, group = s)) +
  geom_line(size = 1.5, alpha = 0.75) +
  scale_color_viridis_c(trans = 'log', breaks = shapes, name = 'Stack shape\nparameter') +
  theme_void() +
  ggtitle('Stack profiles defined by Weibull distribution shape parameter')


shape_plot <- ggplot(
  results_with_params %>% group_by(Stack) %>% filter(max(Fshaft) < 300, 
                                                     clamp_width == 8,
                                                     shim_count %in% 3:8),
  aes(y = U.clk, x = Fshaft, group = Stack, color = shape)
) +
  geom_line(size = 0.5, alpha = 0.3) +
  coord_flip(ylim =  c(0.5,10.5))+
  scale_y_continuous(breaks = seq(0, 11, by = 1)) +
  scale_color_viridis_c(trans = 'log', breaks = shapes, name = 'Stack shape\nparameter') +
  ylab('Shaft velocity (m/sec)') +
  xlab('Force (kgf)') +
  theme_bw() +
  guides(color = 'none') +
  facet_wrap(~ paste('Shim count = ', str_pad(shim_count, width = 2, pad = '0')), scales='free', ncol = 2) 

png(file.path(fig_loc, 'shape_effect.png'), height = 6, width = 10, res = 200, units = 'in')
shape_plot + weib_plot + plot_layout(ncol = 2)
dev.off()


