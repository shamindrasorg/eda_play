set.seed(2019)
library(tidyverse)
library(gganimate)
library(gifski)


# drob approach (minor ammendments) -----
tot_num_rolls <- 5000000
simulation1 <- tibble(roll = 1:tot_num_rolls) %>%
    mutate(result = sample(6, n(), replace = TRUE)) %>%
    crossing(nrolls = seq(10, tot_num_rolls, 10))

simulation2 <- simulation1 %>%
                filter(roll <= nrolls) %>%
                count(nrolls, result)


# purrr approach-----

tot_num_rolls <- 5000000
dice_sides <- 1:6
all_rolls <- base::sample(x = dice_sides, size = tot_num_rolls, replace = TRUE)
filt_anim_rolls <- seq(10, tot_num_rolls, 10)

cum_rolls_count <- function(dice_rolls, roll_value){
    base::return(cumsum(dice_rolls == roll_value))
}

# sim_purr <- purrr::map_dfc(.x = dice_sides,
#                            ~ cum_rolls_count(dice_rolls = all_rolls,
#                                              roll_value = .x)) %>%
#                 magrittr::set_colnames(x = .,
#                                        value = as.character(dice_sides)) %>%
#                 dplyr::bind_cols(tibble(nrolls = 1:tot_num_rolls)) %>%
#                 dplyr::select(nrolls, everything()) %>%
#                 tidyr::gather(data = ., result, n, as.character(dice_sides)) %>%
#                 dplyr::mutate(result = base::as.integer(result)) %>%
#                 dplyr::inner_join(x = .,
#                                   y = tibble::tibble(nrolls = filt_anim_rolls),
#                                   by = c("nrolls"))


sim_purr <- purrr::map_dfc(.x = dice_sides,
                           ~ cum_rolls_count(dice_rolls = all_rolls,
                                             roll_value = .x)) %>%
    magrittr::set_colnames(x = .,
                           value = as.character(dice_sides)) %>%
    dplyr::bind_cols(tibble(nrolls = 1:tot_num_rolls), .) %>%
    tidyr::gather(data = ., result, n, as.character(dice_sides)) %>%
    dplyr::mutate(result = base::as.integer(result)) %>%
    dplyr::inner_join(x = .,
                      y = tibble::tibble(nrolls = filt_anim_rolls),
                      by = c("nrolls"))

ggplot(sim_purr, aes(result, n, fill = factor(result))) +
    geom_col(position = "identity", show.legend = FALSE) +
    transition_manual(nrolls) +
    coord_cartesian(ylim=c(0, tot_num_rolls)) +
    scale_y_continuous(breaks=seq(0, tot_num_rolls, 1000)) +
    view_follow() +
    scale_x_continuous(breaks = 1:6) +
    labs(y = "# of rolls with this result",
         title = "Distribution of results after { current_frame } rolls of a six-sided die")