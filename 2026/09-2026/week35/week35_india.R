# =============================================================================
# TidyTuesday 2026-09-01 | World Castles, Fortresses and Palaces
# Forts and Palaces of India - static map + interactive girafe map
# =============================================================================


# Load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(showtext)
library(ggtext)
library(ggiraph)
library(ggrepel)
library(glue)
library(htmltools)
library(rnaturalearth)


# Options -----------------------------------------------------------------

# Wikimedia serves any width via the ?width= parameter, so the tooltip
# thumbnails are requested small rather than at the 1280px the raw data
# points to. Set embed_images = TRUE to download each photo once and inline
# it as base64 instead - that makes the .html self-contained (works offline,
# survives Wikimedia URL changes) at the cost of a much larger file.
thumb_width  <- 320
embed_images <- FALSE
out_dir      <- file.path("2026", "09-2026", "week35")
image_cache  <- file.path(out_dir, "images")


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-09-01")
world_castles <- tuesdata$world_castles

# Natural Earth's default country polygon shows India *without* the territory
# India claims. Natural Earth publishes point-of-view editions for exactly
# this reason; the "_ind" file is the Indian POV and includes Jammu &
# Kashmir, Aksai Chin and Arunachal Pradesh. POV editions exist only at 10m.
india <- ne_download(
  scale       = 10,
  type        = "admin_0_countries_ind",
  category    = "cultural",
  returnclass = "sf"
) |>
  filter(ADM0_A3 == "IND")


# Load fonts --------------------------------------------------------------

# Pinyon Script is a copperplate script - beautiful as a display face, but
# unreadable below ~14pt, so it carries the title only. EB Garamond handles
# everything that actually has to be read.
font_add_google("Pinyon Script")
font_add_google("EB Garamond")
showtext_opts(dpi = 300)

title_font <- "Pinyon Script"
body_font  <- "EB Garamond"


# Define colours ----------------------------------------------------------

bg_col   <- "#f3e2c7"  # parchment
land_col <- "#e4c9a0"  # sandstone
text_col <- "#4a1c14"  # deep maroon

cat_cols <- c(
  fortress = "#8a2b0f",
  palace   = "#c07a24",
  castle   = "#3d5a6c",
  ruin     = "#7a7364"
)


# Data wrangling ----------------------------------------------------------

plot_data <- world_castles |>
  filter(iso == "IN") |>
  select(
    qid, name, category, lat, lon, year, century,
    wikipedia, image, pageviews, fame_rank
  ) |>
  mutate(
    category = factor(category, levels = names(cat_cols)),
    # 78 of the 156 Indian sites have no build year but most still carry a
    # century, so fall back to that before giving up.
    built = case_when(
      !is.na(year)    ~ as.character(year),
      !is.na(century) ~ century,
      TRUE            ~ "date unrecorded"
    ),
    thumb = str_replace(image, "width=\\d+", paste0("width=", thumb_width))
  )


# Optionally cache the photos locally and inline them ----------------------

if (embed_images) {
  dir.create(image_cache, showWarnings = FALSE, recursive = TRUE)

  local_uri <- function(url, qid) {
    dest <- file.path(image_cache, paste0(qid, ".jpg"))
    if (!file.exists(dest)) {
      try(download.file(url, dest, mode = "wb", quiet = TRUE), silent = TRUE)
    }
    if (file.exists(dest) && file.size(dest) > 0) {
      base64enc::dataURI(file = dest, mime = "image/jpeg")
    } else {
      NA_character_
    }
  }

  plot_data <- plot_data |>
    mutate(thumb = map2_chr(thumb, qid, local_uri)) |>
    mutate(thumb = coalesce(thumb, image))
}


# Build the tooltip -------------------------------------------------------

plot_data <- plot_data |>
  mutate(
    tooltip = as.character(glue(
      "<b>{name}</b><br>",
      "<i>{str_to_title(category)}</i> &middot; {built}<br>",
      "<img src='{thumb}' width='{thumb_width}' ",
      "alt='Photograph of {name}'>"
    )),
    # Clicking a marker opens its Wikipedia article in a new tab.
    onclick = if_else(
      is.na(wikipedia),
      NA_character_,
      as.character(glue('window.open("{wikipedia}")'))
    )
  )

# Lambert conformal conic with standard parallels set for India - keeps the
# north-south shape honest on a portrait canvas.
india_crs <- "+proj=lcc +lat_1=12 +lat_2=32 +lat_0=22 +lon_0=80 +datum=WGS84"

india_proj <- st_transform(india, india_crs)

plot_sf <- plot_data |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(india_crs)

# Label only the handful everyone recognises, ranked by Wikipedia pageviews.
label_sf <- plot_sf |>
  slice_max(pageviews, n = 7) |>
  mutate(
    x = st_coordinates(geometry)[, 1],
    y = st_coordinates(geometry)[, 2]
  )


# Define text -------------------------------------------------------------

n_sites <- nrow(plot_data)
n_forts <- sum(plot_data$category == "fortress")

title <- "Forts and Palaces of India"

# Use ordinary spaces around the <b> tags. gridtext renders `&nbsp;` (and the
# literal U+00A0) at *zero* width, so reaching for a non-breaking space to
# protect the gap is what actually closes it up.
st <- glue(
  "Wikidata lists <b>{n_sites}</b> castles, fortresses, palaces and ruins ",
  "across India, <b>{n_forts}</b> of them fortresses. The densest cluster ",
  "is the Deccan and the Konkan coast, where the Maratha and Deccan ",
  "sultanates fortified every ridge and headland they held. The true count ",
  "runs to many hundreds more."
)

cap <- glue(
  "Graphic: Sharfudeen Yasar Arafath | ",
  "Data: Wikidata via TidyTuesday 2026-09-01 | ",
  "Boundaries: Natural Earth (Indian point of view)"
)


# Plot --------------------------------------------------------------------

g_int <- ggplot() +
  geom_sf(
    data      = india_proj,
    fill      = land_col,
    colour    = text_col,
    linewidth = 0.25
  ) +
  # A soft halo so markers stay legible where they pile up in Rajasthan.
  geom_sf(
    data   = plot_sf,
    colour = bg_col,
    size   = 2.6,
    alpha  = 0.8
  ) +
  geom_sf_interactive(
    data    = plot_sf,
    mapping = aes(
      colour  = category,
      shape   = category,
      data_id = qid,
      tooltip = tooltip,
      onclick = onclick
    ),
    size   = 1.9,
    stroke = 0.9
  ) +
  geom_text_repel(
    data    = label_sf,
    mapping = aes(x = x, y = y, label = name),
    family  = body_font,
    colour  = text_col,
    size    = 2.5,
    seed    = 42,
    # A parchment-coloured halo lifts the labels off the marker field.
    bg.color           = bg_col,
    bg.r               = 0.14,
    box.padding        = 0.7,
    point.padding      = 0.4,
    min.segment.length = 0,
    segment.colour     = text_col,
    segment.size       = 0.2,
    max.overlaps       = Inf
  ) +
  scale_colour_manual(values = cat_cols, labels = str_to_title, drop = FALSE) +
  scale_shape_manual(
    values = c(16, 17, 15, 18),
    labels = str_to_title,
    drop   = FALSE
  ) +
  labs(
    title    = title,
    subtitle = st,
    caption  = cap
  ) +
  theme_void(base_size = 12, base_family = body_font) +
  theme(
    # Generous top margin: Pinyon Script's ascenders overshoot the line box
    # that gridtext measures, and clip against the canvas edge without it.
    plot.margin           = margin(22, 10, 10, 10),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.background       = element_rect(fill = bg_col, colour = bg_col),
    panel.background      = element_rect(fill = bg_col, colour = NA),
    plot.title = element_textbox_simple(
      colour = text_col,
      halign = 0.5,
      margin = margin(t = 4, b = 12),
      family = title_font,
      size   = rel(2.0)
    ),
    plot.subtitle = element_textbox_simple(
      colour     = text_col,
      halign     = 0,
      margin     = margin(b = 6),
      size       = rel(0.72),
      lineheight = 1.35,
      family     = body_font
    ),
    plot.caption = element_textbox_simple(
      colour     = text_col,
      halign     = 0,
      margin     = margin(t = 6),
      # Below ~8pt showtext rounds the space advance down to zero pixels and
      # words start colliding, so the caption does not shrink further.
      size       = rel(0.72),
      lineheight = 1.3,
      family     = body_font
    ),
    # Sat inside the panel the legend lands on the Konkan fort cluster, so
    # it runs along the bottom instead.
    legend.title     = element_blank(),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.margin    = margin(t = -4, b = 0),
    legend.key       = element_rect(colour = NA, fill = "transparent"),
    legend.key.spacing.x = unit(6, "pt"),
    legend.text      = element_text(size = rel(0.8), colour = text_col)
  )


# Save static -------------------------------------------------------------

# showtext renders the Google fonts into the raster device. It is switched on
# only around ggsave: girafe writes SVG through systemfonts, and leaving
# showtext active mangles the text in that pipeline.
showtext_auto(TRUE)

ggsave(
  filename = file.path(out_dir, "week35_india_castles.png"),
  plot     = g_int,
  width    = 5,
  height   = 7,
  units    = "in",
  dpi      = 300,
  bg       = bg_col
)

showtext_auto(FALSE)


# Interactive -------------------------------------------------------------

tooltip_css <- glue("
  padding: 8px;
  font-family: 'EB Garamond', Garamond, serif;
  font-size: 13px;
  line-height: 1.4;
  background-color: {bg_col};
  color: {text_col};
  border: solid {text_col};
  border-radius: 6px;
  border-width: 1.5px;
  box-shadow: 0 3px 10px rgba(0,0,0,0.25);
")

gg_widget <- girafe(
  ggobj      = g_int,
  bg         = bg_col,
  width_svg  = 6,
  height_svg = 8.4,
  options = list(
    opts_tooltip(
      css             = tooltip_css,
      opacity         = 0.98,
      delay_mouseover = 150,
      use_fill        = FALSE
    ),
    opts_hover(css = "opacity: 1;"),
    opts_hover_inv(css = "opacity: 0.25;"),
    opts_selection(type = "none"),
    opts_toolbar(saveaspng = TRUE, position = "topright"),
    opts_zoom(min = 1, max = 5)
  )
)

# girafe writes font-family names into the SVG but does not ship the font, so
# the webfont is linked into the page for the browser to resolve them.
gg_widget <- htmlwidgets::prependContent(
  gg_widget,
  tags$link(
    rel  = "stylesheet",
    href = paste0(
      "https://fonts.googleapis.com/css2",
      "?family=Pinyon+Script",
      "&family=EB+Garamond:ital,wght@0,400;0,600;1,400",
      "&display=swap"
    )
  )
)

# A self-contained .html (one file, no sidecar folder) is produced by pandoc,
# which RStudio and Quarto both bundle but a plain R session does not see on
# its PATH. Point rmarkdown at Quarto's copy when it is there; if no pandoc
# turns up, fall back to the sidecar-folder form rather than erroring out.
if (!rmarkdown::pandoc_available()) {
  quarto_tools <- "C:/Program Files/Quarto/bin/tools"
  if (file.exists(file.path(quarto_tools, "pandoc.exe"))) {
    Sys.setenv(RSTUDIO_PANDOC = quarto_tools)
    rmarkdown::find_pandoc(cache = FALSE)
  }
}

self_contained <- rmarkdown::pandoc_available()

htmlwidgets::saveWidget(
  gg_widget,
  file = normalizePath(
    file.path(out_dir, "week35_india_castles.html"),
    mustWork = FALSE
  ),
  selfcontained = self_contained,
  title         = "Forts & Palaces of India"
)

# saveWidget leaves its 96-file dependency folder behind even once pandoc has
# inlined everything into the .html, so clear it and keep the week to one file.
widget_libs <- file.path(out_dir, "week35_india_castles_files")
if (self_contained && dir.exists(widget_libs)) {
  unlink(widget_libs, recursive = TRUE)
}

gg_widget
