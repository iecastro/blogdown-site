# plot theme
theme_set(
  theme_minimal(base_size = 12,
                base_family = "serif") +
    theme(axis.text = element_text(color = "black"),
          axis.title.y = element_text(hjust = .95))
)

# plot returns
plot_returns <- function(returns, portfolio){
  ggplot(returns,
         aes(date, Ra)) +
    geom_line(aes(color = symbol,
                  group = symbol),
              size = 1,
              alpha = .8) +
    geom_line(data = portfolio,
              aes(date, returns),
              size = 2, lty = "dashed") +
    labs(x = NULL, y = "Returns",
         color = NULL) +
    guides(color = guide_legend(
      title = NULL,
      override.aes = list(size = 2,
                          alpha = 1))) +
    scico::scale_color_scico_d() +
    scale_y_continuous(labels = scales::percent)
}


# plot growth
plot_growth <- function(df){
  
  ggplot(df, aes(date, returns)) +
    geom_line(size = 2) +
    labs(x = NULL, y = NULL,
         color = NULL) +
    scale_y_continuous(labels = scales::dollar) +
    geom_smooth(method = "loess")
  
}
