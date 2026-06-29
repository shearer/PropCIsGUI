library(shiny)
library(bslib)
library(PropCIs)
library(ggplot2)
library(DT)
library(patchwork)

ci_row <- function(measure, method, estimate, lower, upper) {
  data.frame(
    Measure  = measure,
    Method   = method,
    Estimate = round(estimate, 4),
    Lower    = round(lower,    4),
    Upper    = round(upper,    4),
    Width    = round(upper - lower, 4),
    stringsAsFactors = FALSE
  )
}

ci_plot <- function(df, title, null_vals = NULL) {
  df$Label <- factor(paste(df$Method), levels = rev(paste(df$Method)))
  p <- ggplot(df, aes(y = Label, x = Estimate, xmin = Lower, xmax = Upper,
                      color = Measure)) +
    geom_errorbarh(height = 0.35, linewidth = 1) +
    geom_point(size = 3) +
    labs(x = "Estimate", y = NULL, title = title, color = NULL) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          legend.position  = "bottom")
  if (!is.null(null_vals)) {
    for (v in null_vals) {
      p <- p + geom_vline(xintercept = v, linetype = "dashed",
                          color = "grey50", linewidth = 0.6)
    }
  }
  if (length(unique(df$Measure)) > 1) {
    p <- p + facet_wrap(~ Measure, scales = "free_x", ncol = 1) +
      theme(legend.position = "none")
  }
  p
}
