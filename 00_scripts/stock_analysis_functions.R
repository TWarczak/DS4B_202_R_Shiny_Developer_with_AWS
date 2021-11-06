get_stock_list <-
function(stock_index = "SP500") {

  tq_index("SP500") %>%
    select(symbol, company) %>%
    arrange(symbol) %>%
    mutate(label = str_c(symbol, company, sep = ", ")) %>%
    select(label)
}
get_symbol_from_user_input <-
function(user_input) {
  user_input %>% str_split(pattern = ", ") %>% pluck(1, 1)
}
get_stock_data <-
function(stock_symbol,
         from = today() - days(180),
         to   = today(),
         mavg_short = 20,
         mavg_long  = 50) {

  stock_symbol %>%
    tq_get(get = "stock.prices", from = from, to = to) %>%
    select(date, adjusted) %>%
    mutate(mavg_short = rollmean(adjusted, k = mavg_short, fill = NA, align = "right")) %>%
    mutate(mavg_long  = rollmean(adjusted, k = mavg_long, fill = NA, align = "right"))
}
plot_stock_data <-
function(data) {

  g <- data %>%
    pivot_longer(names_to  = "legend",
                 cols      = adjusted:mavg_long,
                 values_to = "value",
                 names_ptypes = list(legend = factor(levels = c("adjusted", "mavg_short", "mavg_long")))) %>%
    dplyr::arrange(legend) %>%
    ggplot(aes(date, value, color = legend, group = legend)) +
    geom_line(aes(linetype = legend)) +
    theme_tq() +
    scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10)) +
    scale_color_tq() +
    labs(y = "Adjusted Share Price", x = "")

  ggplotly(g)
}
generate_commentary <-
function(data, user_input) {

  warning_signal <- data %>%
    tail(1) %>%
    mutate(mavg_warning_flag = mavg_short < mavg_long) %>%
    pull(mavg_warning_flag)

  n_short <- data %>% pull(mavg_short) %>% is.na() %>% sum() + 1
  n_long <- data %>% pull(mavg_long) %>% is.na() %>% sum() + 1

  if (warning_signal) {
    str_glue("in reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day moving average, indicating negative trends")
  } else {
    str_glue("in reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day moving average, indicating positive trends")
  }
}
