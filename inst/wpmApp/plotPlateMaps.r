platemap <- read.csv(file.path("data","platemap.csv"))

library(dplyr)

platemap <- mutate(platemap,
                   Row=as.numeric(match(toupper(substr(Well, 1, 1)), LETTERS)),
                   Column=as.numeric(substr(Well, 2, 5)))
library(ggplot2)


theme_bdc_microtiter <- function(base_size = 12, base_family = "") {
  t <- theme_bdc_grey(base_size = base_size, base_family = base_family,
                      grid.x = FALSE, grid.y = FALSE,
                      gridmin.x = FALSE, gridmin.y = FALSE,
                      ticks.x = FALSE, ticks.y = FALSE,
                      pmargin = base_size / 2) %+replace%
    theme(
      panel.spacing = unit(0, units = "pt"),
      axis.title = element_blank(),
      axis.text = element_text(size = rel(1.0), face = "bold"),
      axis.text.y = element_text(
        margin = margin(r = 0.4 * base_size, l = 0.4 * base_size)
      ),
      axis.text.x = element_text(
        margin = margin(t = 0.4 * base_size, b = 0.4 * base_size)
      ),
      axis.ticks.length = unit(0, "pt"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.spacing = unit(6, "pt"),
      plot.title = element_text(
        size = rel(1.2),
        face = "bold",
        hjust = 0.5,
        margin = margin(b = (base_size / 2) * 1.2)
      ),
      plot.subtitle = element_text(
        size = rel(0.8),
        color = "grey50",
        hjust = 0.5,
        margin = margin(b = (base_size / 2) * 1.2)
      )
    )

  t
}



ggplot(data=platemap, aes(x=Column, y=Row)) +
  geom_point(data=expand.grid(seq(1, 12), seq(1, 8)), aes(x=Var1, y=Var2),
             color="grey90", fill="white", shape=21, size=6) +
  geom_point(size=10) +
  coord_fixed(ratio=(13/12)/(9/8), xlim = c(0.5, 12.5), ylim=c(0.5, 8.5)) +
  scale_y_reverse(breaks=seq(1, 8), labels=LETTERS[1:8]) +
  scale_x_continuous(breaks=seq(1, 12)) +
  labs(title="Plate Layout for My Experiment") +
  theme_bdc_microtiter()
