source('https://raw.githubusercontent.com/mosscoder/restackor_opt/main/ro_funs.R') # restackor opt code

library(tidyverse)
library(patchwork)

results_loc <- 'Z:\\Downloads\\restackor_results' # folder to save results

load_database_params('charger_2_1_base') # load the charger 2.1 damper properties into ReStackor

set_adjusters(n_click = 9, # set the adjusters
              preload = 2,
              u_wheel = 10)

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
  mutate(Stack = 'Stock Charger 2.1')

candidate_result <- tidy_restackor_results(file.path(results_loc, 'candidate.csv')) %>% 
  mutate(Stack = 'Candidate')

all_results <- rbind(stock_result, candidate_result)

all_stacks <- rbind(stock_charger_stack %>% mutate(Stack = 'Stock Charger 2.1'),
                    candidate_stack %>% mutate(Stack = 'Candidate')
                    )

p1 <- ggplot(
  all_results,
  aes(
    y = U.clk,
    ymin = U.wo,
    ymax = U.clsd,
    x = Fshaft,
    group = Stack,
    fill = Stack,
    color = Stack
  )
) +
  geom_ribbon(size = 0, alpha = 0.2) +
  geom_line(size = 1, alpha = 1) +
  coord_flip()+
  ylab('Shaft velocity (m/sec)') +
  xlab('Force (kgf)') +
  theme_bw() +
  theme(legend.position = 'top') +
  ggtitle("Produced with www.shimrestackor.com models 
          and libraries from https://github.com/mosscoder/restackor_opt/")

p2 <- ggplot(all_stacks %>% 
               group_by(Stack) %>% 
               mutate(n = row_number()) %>% 
               filter(n != max(n)), aes(y = width/2, x = factor(n), width = thickness*1.5)) +
  geom_bar(stat = 'identity', fill = 'black') +
  geom_label(aes(x = factor(n), y = 1, label = paste(width, thickness, sep = 'x')),
             size = 2) +
  facet_wrap(~Stack, ncol = 1) +
  coord_flip() +
  theme_classic() +
  xlab('') +
  ylab('Width (mm)')
  

png(file.path(results_loc, 'stock_charger_force_plot.png'), 
    height = 5, width = 9, res = 200, units = 'in')
p1 + p2 + plot_layout(width = c(3, 1))
dev.off()