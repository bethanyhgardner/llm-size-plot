# Setup----
library(tidyverse)  # general data wrangling
library(googlesheets4)  # download file from Google Sheets
library(janitor)  # convert column names to play nice with R
library(scales)  # abbreviate axes with M/B/T labels
library(ggrepel)  # push labels away from points
library(patchwork)  # combine plots
library(ggplotify)  # turn patchwork back into ggplot

gs4_deauth()  # only accessing public files


# Data----
# Size data & sources are in anyone-with-the-link-can-view Google Sheet
data_url <- str_c(
  "https://docs.google.com/spreadsheets/d/",
  "1HGxA9URfEeUYp48qNXMMOPBAt3a18mSsdYqI5WbeE5w/edit#gid=0"
)

# Download it, drop source columns, and save offline copy as CSV
read_sheet(data_url, col_types = "ciTccccc---") |>  # first 8 columns only
  clean_names() |>  # convert column names to snake_case
  write_csv("data.csv")  # export

# Set up df
df <- read_csv("data.csv", col_types = "fiTffffc") |>  # load CSV just created
  filter(include == "x") |>  # subset models to plot (mostly largest in family)
  mutate(arxiv_date = as.Date(arxiv_date)) |>  # Date type to work with {scales}
  mutate(  # group all "Other Company" and specify factor order
    company = factor(
      case_when(
        str_starts(company, "Other Company") ~ "Other Company",
        company == "Open Source/Academic" ~ "Academic",
        .default = company
      ),
      levels = c("Google", "Meta", "OpenAI", "Other Company", "Academic")
    )
  ) |>
  rename(parameters_chr = parameters) |>
  mutate(  # get N parameters as numeric col from abbreviation in char col
    parameters_num = as.numeric(str_sub(parameters_chr, end = -2)),
    parameters_mult = case_when(
      str_sub(parameters_chr, -1) == "M" ~ 1000000,
      str_sub(parameters_chr, -1) == "B" ~ 1000000000,
      str_sub(parameters_chr, -1) == "T" ~ 1000000000000
    ),
    parameters_num = parameters_num * parameters_mult
  ) |>
  select(-include, -parameters_mult)


# Plot----
## Full size range----
plot_full <- ggplot(df) +  # specifying aes() in each geom below
  geom_rect(  # first draw grey background in region that will get zoomed in
    data = tibble(  # geom_rect needs min/max for X and Y coordinates
      xmin = as.Date("2017-11-01"),  # add two months extra on each side so the
      xmax = as.Date("2024-08-01"),  # grey rectangle extends to the very edges
      ymin = -20e9, ymax = 20e9  # band is around 100M range
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "grey75", color = "grey75"
  ) +
  geom_point(  # draw point for each model
    aes(
      x = arxiv_date,  # date on X axis
      y = parameters_num,  # size on Y axis
      color = company,  # color code by company
      fill = company  # add this to use rectangle instead of dot key_glpyh
    ),
    size = 1,  # make slightly bigger than default
    key_glyph = "rect"  # rectangle legend key instead of dot or letter in box
  ) +
  geom_label_repel(  # label each point, same aesthetics as geom_point
    aes(x = arxiv_date, y = parameters_num, label = name, color = company),
    size = 3,  # text size
    box.padding = unit(0.02, "lines"),  # padding in text box
    segment.size = 0.5,  # line size
    min.segment.length = 0,  # always draw line from text to point
    force = 25,  # push labels away a bit more
    max.overlaps = 13,  # allow more overlaps
    seed = 2024,  # to replicate random aspect of fit
    show.legend = FALSE  # just use legend hacked in geom_point layer
  ) +
  scale_color_brewer(palette = "Set1") +  # color palette from RColorBrewer
  scale_fill_brewer(palette = "Set1") +
  scale_x_date(  # X axis labels
    date_breaks = "year",  # labels at each year
    date_labels = "%Y",  # just label year, not 01/01/year
    limits = c(as.Date("2017-11-01"), as.Date("2024-08-01")),  # time range
    expand = c(0, 0)  # space here would add space to L/R of the grey rectangle
  ) +
  scale_y_continuous(  # Y axis labels
    limits = c(-1e11, 1.975e12),  # expand range on bottom for labels
    expand = c(0.1, 0.1),  # keep a little space on the edges
    breaks = c(1e8, 1e11, 5e11, 1e12, 2e12),  # define breaks
    labels = label_number(scale_cut = cut_short_scale())  # label with M/B/T
  ) +
  theme_classic(base_size = 12) +  # start with white background theme
  theme(
    axis.text = element_text(size = 11),  # axis text and title size
    axis.title = element_text(size = 12),
    legend.key.size = unit(0.0225, "snpc"),  # make legend squares smaller
    legend.key.spacing.y = unit(0.01, "snpc"),  # add vertical space
    legend.text = element_text(size = 11),  # legend text size same as axes
    legend.title = element_text(size = 12),
    legend.position = "inside",  # move legend from right to inside
    legend.position.inside = c(0.125, 0.725),  # put in the top left corner
    panel.grid.major = element_line(color = "grey95")  # gray lines along axis
  ) +
  labs(  # set labels
    x = "Release Date",
    y = "Number of Parameters",
    color = "Company",
    fill = "Company"
  )


## Zoomed in range----
plot_subset <- df |>
  filter(parameters_num <= 15e9) |>  # just models under 15B parameters
  ggplot(aes(
    x = arxiv_date,
    y = parameters_num,
    label = name,
    color = company
  )) +
  geom_point(size = 1) +
  geom_label_repel(
    size = 3,
    box.padding = unit(0.02, "lines"),
    segment.size = 0.5,
    min.segment.length = 0,
    force = 12,
    seed = 5202024
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_date(
    date_breaks = "year",
    date_labels = "%Y",
    expand = c(0.02, 0.02),
    limits = c(as.Date("2017-11-01"), as.Date("2024-08-01"))
  ) +
  scale_y_continuous(
    limits = c(-1e9, 15e9),
    expand = c(0.01, 0.01),
    breaks = c(100e6, 1e9, 2.5e9, 5e9, 7.5e9, 10e9, 12.5e9, 15e9),
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.line = element_blank(),  # full border instead of axis lines
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.border = element_rect(color = "black", linewidth = 1, fill = NA),
    panel.grid.major = element_line(color = "grey95")
  ) +
  labs(x = element_blank(), y = element_blank()) +
  guides(color = guide_none())  # only use legend in bigger plot


## Combine and annotate----
# Add plots together using {patchwork}
plot_combined <- plot_full + plot_subset +
  plot_layout(  # defining layout using grid coordinates
    design = c(
      area(t = 1, b = 6, l = 1, r = 10),
      area(t = 7, b = 10, l = 2, r = 9)
    )
  )

# Use {ggplotify} to Convert the patchwork object back to a ggglot
plot_combined <- as.ggplot(plot_combined)

# To use annotate() to add an arrow on top of both plots (not just one)
plot_combined <- plot_combined +
  annotate(
    geom = "segment",  # draw line segment
    arrow = arrow(type = "closed", length = unit(0.02, "snpc")),  # with arrow
    linewidth = 1.75, color = "grey75",  # make thick and medium gray
    x = 0.15, y = 0.52,  # (0, 0) is bottom left of combined plots
    xend = 0.23, yend = 0.40
  )


## Export---
# Save as PNG, noting that changing the size may change the results of
# geom_label_repel() calculating the label locations and if the gray arrow is
# aligned. Also note that geom_label_repel() might throw warnings when
# rendering to display in RStudio, but it should not warning about unlabeled
# data points when rendering to the full size file here.
ggsave(
  plot = plot_combined, filename = "llm_size_plot.png",
  width = 10, height = 10, unit = "in", device = "png", dpi = 300
)
