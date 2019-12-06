library(ggplot2)
library(dplyr)

platemap <- read.csv(file.path("data","platemap.csv"), stringsAsFactors = FALSE)

test_df <- platemap[1:2,]

test_df <- rbind(test_df, c("A1","A",2))
test_df <- rbind(test_df, c("A2", "blank", 0))
test_df <- rbind(test_df, c("B1", "forbidden", 1))

test_df <- mutate(test_df,
                   Row=as.numeric(match(toupper(substr(Well, 1, 1)), LETTERS)),
                   Column=as.numeric(substr(Well, 2, 5)))
test_df$Environment <- as.factor(test_df$Environment)
test_df$Well <- as.factor(test_df$Well)
test_df$Strain <- as.factor(test_df$Strain)
test_df$individu <- c("117","211", "314", NA, NA)
plate_lines <- 8
plate_cols <- 12

# cette palette permet de colorier selon que c'est un blank, une case interdite, ou un groupe
palette_strains <- c("blank"="grey", "forbidden"="red")
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

names(cbbPalette) <- levels(test_df$Strain)
palette_strains <- c(palette_strains, cbbPalette)
colScale <- scale_colour_manual(name = "grp",values = palette_strains)

ggplot(data=test_df, aes(x=Column, y=Row)) +
  geom_point(data=expand.grid(seq(1, plate_cols), seq(1, plate_lines)), aes(x=Var1, y=Var2),
             color="grey90", fill="white", shape=21, size=6) +
  geom_point(aes(shape=Environment, colour=Strain), size=7.5) +
  geom_text(aes(label = individu), size=2) +
  colScale +
  coord_fixed(ratio=(13/plate_cols)/(9/plate_lines), xlim = c(0.9, plate_cols+0.1), ylim=c(0, plate_lines+1)) +
  scale_y_reverse(breaks=seq(1, plate_lines), labels=LETTERS[1:plate_lines]) +
  scale_x_continuous(breaks=seq(1, plate_cols)) +
  labs(title="Plate Layout for My Experiment") +
  theme_bdc_microtiter()





theme_bdc_grey <- function(base_size = 12, base_family = "",
                           base_grey = "grey70",
                           grid.x = FALSE, grid.y = FALSE,
                           gridmin.x = grid.x, gridmin.y = grid.y,
                           ticks.x = TRUE, ticks.y = TRUE,
                           pmargin = base_size / 2) {
  half_line <- base_size / 2
  quarter_line <- base_size / 4
  line_size <- 0.5
  medgrey <- "grey40"

  t <- theme(
    line = element_line(color = base_grey, size = line_size,
                        linetype = 1, lineend = "square"),
    rect = element_rect(fill = "white", color = base_grey,
                        size = line_size, linetype = 1),
    text = element_text(family = base_family, face = "plain",
                        color = "black", size = base_size,
                        lineheight = 0.9, hjust = 0.5, vjust = 0.5,
                        angle = 0, margin = margin(), debug = FALSE),

    axis.line = element_blank(),
    axis.text = element_text(size = rel(0.8), color = medgrey),
    axis.text.x = element_text(
      margin = margin(t = 0.8 * quarter_line),
      vjust = 1
    ),
    axis.text.y = element_text(
      margin = margin(r = 0.8 * quarter_line),
      hjust = 1
    ),
    axis.ticks = element_line(size = line_size / 2),
    axis.ticks.x = element_line(
      size = ifelse(ticks.x, line_size / 2, 0)
    ),
    axis.ticks.y = element_line(
      size = ifelse(ticks.y, line_size / 2, 0)
    ),
    axis.ticks.length = unit(quarter_line, units = "pt"),
    axis.title.x = element_text(
      margin = margin(t = 0.8 * half_line, b = 0.8 * quarter_line)
    ),
    axis.title.y = element_text(
      angle = 90,
      margin = margin(r = 0.8 * half_line, l = 0.8 * quarter_line)
    ),

    legend.background = element_rect(fill = "transparent", color = NA),
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing = unit(0, units = "cm"),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.key.size = unit(0.5, units = "lines"),
    legend.key.height = unit(0.5, units = "lines"),
    legend.key.width = unit(1.0, units = "lines"),
    legend.text = element_text(size = rel(0.6), hjust = 0, vjust = 0.5,
                               color = medgrey),
    legend.text.align = NULL,
    legend.title = element_text(size = rel(0.6), face = "bold",
                                vjust = 0.5),
    legend.title.align = 1,
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.box.margin = margin(0, 0, 0, 0, unit = "pt"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(6, units = "pt"),

    panel.background = element_rect(fill = "white", color = base_grey),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = NA, size = line_size / 5),
    panel.grid.major.x = element_line(
      color = ifelse(grid.x, base_grey, NA)
    ),
    panel.grid.major.y = element_line(
      color = ifelse(grid.y, base_grey, NA)
    ),
    panel.grid.minor = element_line(color = NA, size = line_size / 10),
    panel.grid.minor.x = element_line(
      color = ifelse(gridmin.x, base_grey, NA)
    ),
    panel.grid.minor.y = element_line(
      color = ifelse(gridmin.y, base_grey, NA)
    ),
    panel.spacing = unit(quarter_line, units = "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,

    strip.background = element_rect(fill = "transparent", color = NA),
    strip.text = element_text(
      color = "grey40",
      size = rel(0.8),
      face = "bold"
    ),
    strip.text.x = element_text(
      margin = margin(t = quarter_line, b = quarter_line)
    ),
    strip.text.y = element_text(
      angle = -90,
      margin = margin(l = quarter_line, r = quarter_line)
    ),
    strip.switch.pad.grid = unit(0.1, units = "cm"),
    strip.switch.pad.wrap = unit(0.1, units = "cm"),

    plot.background = element_rect(color = "transparent"),
    plot.title = element_text(
      size = rel(1.2),
      face = "bold",
      hjust = 0,
      margin = margin(b = half_line * 1.2)
    ),
    plot.subtitle = element_text(
      size = rel(0.8),
      color = "grey40",
      face = "italic",
      hjust = 0,
      margin = margin(b = (base_size / 2) * 1.2)
    ),
    plot.caption = element_text(
      size = rel(0.7),
      color = "grey40",
      face = "plain",
      hjust = 0,
      margin = margin(b = base_size * 0.4, t = base_size * 0.4,
                      r = 0, l = 0)
    ),
    plot.margin = margin(pmargin, pmargin, pmargin, pmargin),
    complete = TRUE
  )

  t
}



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
