# /*===========================================================
#' # Visualization the field and spatial units
# /*===========================================================
# fig.id = "field-layout",
# fig.cap = "Simulated field layout with spatial unit definitions",
# fig.dim = c(6, 6)

field_with_design <- readRDS(here("Data/field_with_design.rds"))
field_sf <- field_with_design[1, ]$field_sf[[1]]

plot_sf <-
  field_sf %>%
  nest_by(plot_id) %>%
  mutate(data = list(
    st_union(data)
  )) %>%
  unnest() %>%
  st_as_sf()

block_sf <-
  field_sf %>%
  nest_by(block_id) %>%
  mutate(data = list(
    st_union(data)
  )) %>%
  unnest() %>%
  st_as_sf()

block_text_sf <- st_centroid(block_sf)

#* Field: plots and blocks
g_field <-
  ggplot() +
  geom_sf(data = plot_sf, size = 0.2, fill = NA) +
  geom_sf(data = filter(plot_sf, plot_id == 264), fill = "red", alpha = 0.5) +
  geom_sf(data = block_sf, fill = NA, size = 1.5) +
  annotate("text", x = 830, y = 74, label = "plot", color = "red", size = 5) +
  geom_segment(
    aes(x = 830, xend = 830, y = 30, yend = -80),
    arrow = arrow(length = unit(0.5, "cm")),
    size = 1,
    color = "red"
  ) +
  geom_sf_text(
    data = block_text_sf,
    aes(label = paste0("block ", block_id)),
    size = 5
  ) +
  theme_void() +
  ggtitle("Panel (a): Plots and blocks in an experimental field")

## inside a plot
plot_sf_focus <- filter(field_sf, plot_id == 1)

subplot_sf <-
  plot_sf_focus %>%
  nest_by(aunit_id, buffer) %>%
  mutate(data = list(
    st_union(data)
  )) %>%
  unnest() %>%
  st_as_sf() %>%
  mutate(label = ifelse(
    buffer == 1,
    "buffer",
    paste0("subplot-", aunit_id - 1)
  )) %>%
  mutate(buf_or_not = ifelse(label == "buffer", "buffer", "subplot"))

subplot_text_sf <- st_centroid(subplot_sf)

site_sf <- plot_sf_focus[7, ]

g_inside_plot <-
  ggplot() +
  geom_sf(data = plot_sf_focus, size = 0.2, fill = NA) +
  geom_sf_text(data = subplot_text_sf, aes(label = label), size = 4) +
  geom_sf(data = subplot_sf, size = 1.2, fill = NA) +
  geom_sf(data = site_sf, fill = "red", alpha = 0.3) +
  annotate("text", x = 39, y = 429, label = "site") +
  scale_fill_discrete(name = "") +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  ggtitle("Panel (b): Subplots, buffers, and sites in a single plot")

g_layout <- g_field / g_inside_plot

# /*===========================================================
#' # Experimental Design
# /*===========================================================
# fig.id = "field-N-design",
# fig.cap = "Experiment design of nitrogen (N) rates"

reg_data <-
  readRDS(here("Data/LatinSquareFixed_144.rds")) %>%
  pull(reg_data) %>%
  .[[1]] %>%
  .[1]

data <- reg_data$data[[1]]

N_levels <- reg_data$N_levels[[1]]

data <-
  data %>%
  .[, Nid := as.numeric(as.factor(Nid))] %>%
  .[, Ntg := N_levels[Nid]]

f <-
  left_join(field_sf, data[, .(aunit_id, Ntg)], by = "aunit_id") %>%
  data.table() %>%
  .[, Ntg := factor(Ntg)] %>%
  .[buffer == 1, Ntg := "buffer"] %>%
  .[, Ntg := factor(Ntg, levels = c("buffer", as.character(levels(Ntg)[-7])))] %>%
  st_as_sf()

g_exp <-
  ggplot() +
  geom_sf(data = f, aes(fill = (Ntg)), size = 0.1) +
  scale_fill_viridis_d(name = "N rate (kg/ha)", direction = -1) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 10)
  )

# /*===========================================================
#' # The value of GWR-based VRA over SCAM-based URA for GWR-R and GWR-T
# /*===========================================================
# fig.id = "pi-dif-dist",
# fig.cap = "The value of GWR-based VRA over SCAM-based URA for GWR-R and GWR-T"

g_value <-
  results %>%
  ggplot(data = .) +
  geom_histogram(aes(x = pi_diff), fill = NA, color = "black") +
  facet_grid(type ~ .) +
  xlab("The value of VRA over URA ($ per ha)") +
  ylab("Number of Simulation Cases")

# /*===========================================================
#' # Comparison of Estimated and True Coefficients
# /*===========================================================
# fig.id = "true-vs-estimated-coef-gwr-r",
# fig.cap = "Comparison of Estimated and True Coefficients"

#* read the simulation data for a sigle simulationused for illustration
single_sim <-
  here("Results", "aunit_sim_single.rds") %>%
  readRDS() %>%
  .[, type := ifelse(transfer == 0, "GWR-R", "GWR-T")]

plot_data <-
  single_sim %>%
  .[type == "GWR-R", ] %>%
  .[, .(aunit_id, b1, b2, b1_hat, b2_hat)] %>%
  melt(id.var = "aunit_id") %>%
  .[, type := ifelse(str_detect(variable, "hat"), "Estimated", "True")] %>%
  .[, variable := gsub("_hat", "", variable)] %>%
  dcast(aunit_id + variable ~ type, value.var = "value")

g_b1 <-
  plot_data[variable == "b1", ] %>%
  ggplot(data = .) +
  geom_point(aes(y = Estimated, x = True), size = 0.3) +
  xlim(0, NA) +
  ylim(0, NA)

g_b2 <-
  plot_data[variable == "b2", ] %>%
  ggplot(data = .) +
  geom_point(aes(y = Estimated, x = True), size = 0.3) +
  geom_hline(yintercept = 0, color = "red") +
  xlim(NA, 0.1) +
  ylim(NA, 0.1)

g_comp_coef <- g_b1 / g_b2

# /*===========================================================
#' # Comparison of Estimated and True EONR
# /*===========================================================
# fig.id = "true-vs-estimated-optn-gwr-r",
# fig.cap = "Comparison of Estimated and True EONR"

g_comp_eonr <-
  single_sim %>%
  .[type == "GWR-R", ] %>%
  .[, .(aunit_id, opt_N, opt_N_gwr)] %>%
  ggplot(data = .) +
  geom_point(aes(y = opt_N_gwr, x = opt_N), size = 0.3) +
  geom_abline(slope = 1, color = "red") +
  xlab("True Optimal Nitrogen Rate (kg/ha)") +
  ylab("Estimated Optimal Nitrogen Rate (kg/ha)") +
  coord_equal()

# /*===========================================================
#' # Bias in the estimation of the value of GWR-based VRA over SCAM-based URA
# /*===========================================================
# fig.id = "bias-est-pi",
# fig.cap = "Bias in the estimation of the value of GWR-based VRA over SCAM-based URA for GWR-R and GWR-T"

g_bias <-
  results %>%
  .[bias < 100, ] %>%
  ggplot(data = .) +
  geom_histogram(aes(x = bias), fill = NA, color = "black") +
  facet_grid(type ~ .) +
  xlab("Bias in the Estimation of the Value of VRA over URA ($ per ha)") +
  ylab("Number of Simulation Cases")

# /*===========================================================
#' # The cause of significant over-estimation of the value of GWR-based VRA
# /*===========================================================
# fig.id = "why-bias-many",
# fig.cap = "The cause of significant over-estimation of the value of GWR-based VRA"

# /*+++++++++++++++++++++++++++++++++++
#' # Prepare parameters
# /*+++++++++++++++++++++++++++++++++++
field_parameters <- readRDS(here("Data/field_parameters.rds"))

pCorn <- field_parameters$pCorn
pN <- field_parameters$pN

# /*+++++++++++++++++++++++++++++++++++
#' # Prepare yield response curves
# /*+++++++++++++++++++++++++++++++++++
il_data_oe <-
  readRDS(here("Results/il_data_oe.rds")) %>%
  .[,
    lapply(.SD, mean),
    by = aunit_id,
    .SDcols = c(
      "b0", "b1", "b2", "Nk",
      "b0_hat", "b1_hat", "b2_hat",
      "opt_N", "opt_N_gwr", "opt_N_scam"
    )
  ]

n_data <-
  data.table(
    N = seq(
      min(il_data_oe$opt_N_gwr) - 50,
      max(il_data_oe$opt_N_gwr, il_data_oe$opt_N_scam),
      length = 30
    )
  )

set.seed(710527)
aunit_id_ls <- sample(il_data_oe$aunit_id, 50)

gwr_curv_data <-
  expand_grid_df(il_data_oe, n_data) %>%
  .[, yield := (b0_hat + b1_hat * N + b2_hat * N^2) / 1000] %>%
  .[, profit := pCorn * yield * 1000 - pN * N] %>%
  .[aunit_id %in% aunit_id_ls, ]

# /*+++++++++++++++++++++++++++++++++++
#' # Prepare point data
# /*+++++++++++++++++++++++++++++++++++

point_data <-
  il_data_oe %>%
  .[, y_hat_gwr := (b0_hat + b1_hat * opt_N_gwr + b2_hat * opt_N_gwr^2) / 1000] %>%
  .[, y_hat_scam := (b0_hat + b1_hat * opt_N_scam + b2_hat * opt_N_scam^2) / 1000] %>%
  .[aunit_id %in% aunit_id_ls, ] %>%
  .[, .(aunit_id, y_hat_gwr, y_hat_scam, opt_N_gwr, opt_N_scam)] %>%
  melt(id.var = "aunit_id") %>%
  .[, var_type := fifelse(str_detect(variable, "y_hat"), "yield", "N")] %>%
  .[, type := fifelse(str_detect(variable, "gwr"), "GWR", "SCAM")] %>%
  .[, variable := NULL] %>%
  dcast(aunit_id + type ~ var_type, value.var = "value") %>%
  .[, profit := pCorn * yield * 1000 - pN * N]

# /*+++++++++++++++++++++++++++++++++++
#' # Figure
# /*+++++++++++++++++++++++++++++++++++
g_why_bias_many <-
  ggplot() +
  geom_line(
    data = gwr_curv_data,
    aes(y = yield, x = N, group = aunit_id),
    size = 0.3,
    color = "grey"
  ) +
  geom_point(
    data = point_data,
    aes(y = yield, x = N, color = type),
    size = 0.6
  ) +
  ylab("Estimated Yield (ton/ha)") +
  xlab("Nitrogen Rate (kg/ha)") +
  theme(
    legend.position = "bottom"
  ) +
  scale_color_discrete(name = "Estiamted EONR")

# /*===========================================================
#' # An illustration of over-estimation of the value of GWR-based VRA over SCAM-based URA
# /*===========================================================
# fig.id = "why-bias-single",
# fig.cap = "An illustration of over-estimation of the value of GWR-based VRA over SCAM-based URA",
# fig.dim = c(6, 7)

# /*+++++++++++++++++++++++++++++++++++
#' # Preapare yeld response curves
# /*+++++++++++++++++++++++++++++++++++
true_curv_data <-
  il_data_oe %>%
  .[aunit_id %in% aunit_id_ls[1], ] %>%
  expand_grid_df(., n_data) %>%
  .[, y_true_curve_raw := (b0 + b1 * N + b2 * N^2) / 1000] %>%
  .[, y_true_curve_pl := (b0 + b1 * Nk + b2 * Nk^2) / 1000] %>%
  .[, yield := fifelse(N < Nk, y_true_curve_raw, y_true_curve_pl)] %>%
  .[, profit := pCorn * yield * 1000 - pN * N] %>%
  .[, .(yield, profit, N)] %>%
  .[, type := "True"]

gwr_curv_data_f <-
  gwr_curv_data[aunit_id %in% aunit_id_ls[1], ] %>%
  .[, .(yield, profit, N)] %>%
  .[, type := "GWR-estimated"]

curv_data <-
  rbind(true_curv_data, gwr_curv_data_f) %>%
  .[, type := factor(type, levels = c("True", "GWR-estimated"))]

# /*+++++++++++++++++++++++++++++++++++
#' # Prepare yield points data
# /*+++++++++++++++++++++++++++++++++++
true_yield_data <-
  il_data_oe %>%
  .[aunit_id %in% aunit_id_ls[1], ] %>%
  .[, .(b0, b1, b2, Nk, opt_N_gwr, opt_N_scam, opt_N)] %>%
  melt(id.vars = c("b0", "b1", "b2", "Nk")) %>%
  setnames("value", "N") %>%
  .[, y_true_raw := (b0 + b1 * N + b2 * N^2) / 1000] %>%
  .[, y_true_pl := (b0 + b1 * Nk + b2 * Nk^2) / 1000] %>%
  .[, yield := fifelse(N < Nk, y_true_raw, y_true_pl)] %>%
  .[, profit := pCorn * yield * 1000 - pN * N] %>%
  .[, type := fcase(
    variable == "opt_N_gwr", "GWR",
    variable == "opt_N_scam", "SCAM",
    variable == "opt_N", "True"
  )] %>%
  .[, .(yield, profit, N, type)]

point_data_gwr <-
  point_data[aunit_id %in% aunit_id_ls[1], ]

# /*+++++++++++++++++++++++++++++++++++
#' # Figure
# /*+++++++++++++++++++++++++++++++++++
g_yield <-
  ggplot() +
  geom_line(data = curv_data, aes(y = yield, x = N, linetype = type)) +
  geom_point(data = true_yield_data, aes(y = yield, x = N, color = type)) +
  geom_point(data = point_data_gwr, aes(y = yield, x = N, color = type)) +
  ylab("Yield (ton/ha)") +
  xlab("Nitrogen Rate (kg/ha)") +
  scale_color_discrete(name = "EONR") +
  scale_linetype_discrete(name = "Yield Response Fnctions") +
  ggtitle("(a) Estimated and True Yields")

g_profit <-
  ggplot() +
  geom_line(data = curv_data, aes(y = profit / 1000, x = N, linetype = type)) +
  geom_point(data = true_yield_data, aes(y = profit / 1000, x = N, color = type)) +
  geom_point(data = point_data_gwr, aes(y = profit / 1000, x = N, color = type)) +
  ylab("Partial Profit ($1000/ha)") +
  xlab("Nitrogen Rate (kg/ha)") +
  scale_color_discrete(name = "EONR") +
  scale_linetype_discrete(name = "Profit Response Fnctions") +
  ggtitle("(b) Estimated and True Profits")

g_why_bias_single <- g_yield / g_profit