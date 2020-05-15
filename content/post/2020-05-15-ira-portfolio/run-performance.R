# monthly returns for individual securities
returns_combined <- tq_get(unique(weights$symbol),  
                           get = "stock.prices",
                           from = start_date) %>% 
  group_by(symbol) %>%
  tq_transmute(adjusted, periodReturn, 
               period = "monthly",
               col_rename = "Ra")

# portfolio returns
get_returns <- function(return_df, weight_df, growth = FALSE){
  
  df <- return_df
  
  x <- weight_df
  
  # filter weights for correct acct
  wt <- filter(weights, 
               acct == x) %>% 
    select(-acct)
  
  tq_portfolio(df,
               assets_col = symbol,
               returns_col = Ra,
               weights = wt,
               col_rename = "returns",
               wealth.index = growth)
}


port_combined <- returns_combined %>% 
  get_returns("combined")

port_ira <- returns_combined %>% 
  filter(!symbol %in%
           c("EPR", "VTR")) %>% 
  get_returns("ira")

port_ind <- returns_combined %>% 
  filter(symbol %in%
           c("EPR", "VTR")) %>% 
  get_returns("ind")

# track growth
growth_ira <- returns_combined %>% 
  filter(!symbol %in%
           c("EPR", "VTR")) %>% 
  get_returns("ira", growth = TRUE)

growth_ind <- returns_combined %>% 
  filter(symbol %in%
           c("EPR", "VTR")) %>% 
  get_returns("ind", growth = TRUE)

source("plot-functions.R")

p_combine <- plot_returns(returns_combined, port_combined) +
  labs(subtitle = "Combined portfolios")

# returns plots
p_ira <- returns_combined %>% 
  filter(!symbol %in%
           c("EPR", "VTR")) %>% 
  plot_returns(port_ira) +
  labs(subtitle = "Roth IRA acct.")

p_ind <- returns_combined %>% 
  filter(symbol %in%
           c("EPR", "VTR")) %>% 
  plot_returns(port_ind) +
  labs(subtitle = "Indv. taxable acct.")

# growth plots
grow_ira <- plot_growth(growth_ira) +
  labs(subtitle = "Roth IRA acct. growth of $1")

grow_ind <- plot_growth(growth_ind) +
  labs(subtitle = "Indv. taxable acct. growth of $1")
