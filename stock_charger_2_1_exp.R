source('https://raw.githubusercontent.com/mosscoder/restackor_opt/main/ro_funs.R') # restackor opt code

library(tidyverse)

results_loc <- 'Z:\\Downloads\\restackor_results' # folder to save results

load_database_params('charger_2_1_base') # load the charger 2.1 damper properties into ReStackor

set_adjusters(n_click = 9, # set the adjusters
              preload = 2,
              u_wheel = 3)

# the stock charger shim stack
stock_charger_stack <- data.frame(width = c(18, 16, 14, 8, 10), 
                                  thickness = c(0.1, 0.1, 0.1, 0.4, 4))

candidate_stack <- data.frame(width = c(18, 18, 18, 16, 14, 8, 10), 
                                  thickness = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.4, 4))

# run restackor.exe
run_shimstack(shim_df = stock_charger_stack,
              outfile = file.path(results_loc, 'stock_charger.csv'))

run_shimstack(shim_df = candidate_stack,
              outfile = file.path(results_loc, 'candidate.csv'))


#plot outcome
stock_result <- tidy_restackor_results(file.path(results_loc, 'stock_charger.csv')) %>% 
  pivot_longer(cols = U.clk:U.clsd,
               names_to = 'setting',
               values_to = 'velocity') %>% 
  mutate(Stack = 'Stock charger')

candidate_result <- tidy_restackor_results(file.path(results_loc, 'candidate.csv')) %>% 
  pivot_longer(cols = U.clk:U.clsd,
               names_to = 'setting',
               values_to = 'velocity') %>% 
  mutate(Stack = 'Candidate')

png(file.path(results_loc, 'stock_charger_force_plot.png'), 
    height = 4, width = 8, res = 200, units = 'in')
ggplot(rbind(stock_result, candidate_result),
       aes(y = Fshaft,
           x = velocity, 
           group = setting,
           linetype = setting,
           color = Stack)
) +
  geom_line(size = 1, alpha = 0.6) +
  scale_linetype_manual(values = c(1,2,2)) +
  guides(linetype = 'none') +
  xlab('Shaft velocity (m/sec)') +
  ylab('Force (kgf)')
dev.off()