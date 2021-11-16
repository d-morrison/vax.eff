#' Title
#'
#' @param pV
#' @param R
#' @param f
#' @param n_points
#'
#' @return
#' @export
#'

#' @importFrom ggplot2 ggplot xlim expand_limits geom_function geom_segment aes annotate theme_bw theme element_text element_blank xlab ylab labs scale_y_continuous sec_axis
#' @importFrom latex2exp TeX
fig1 = function(
  pV,
  R,
  f,
  n_points = 201,
  label_x = 1.05,
  lwd = 1,
  margin_offset = .02
)
{
  f1 = function(pL, rV)
  {
    reporting_multiplier(
      f = f,
      `p(V)` = pV,
      R = R,
      `p(V*|V)` = rV,
      `p(L|V*E*)` = pL) %>% pull(`R*`)

  }

  rVs = c(
    # 0, .1,
    .6, .7, .8, .9, 1)
  rv_labels = paste("\\textit{r_V} =", formatC(rVs, format = "f", digits = 1 ))
  plot_title = paste0(
    "\\overset{Relationship between \\textit{p_L}, \\textit{r_V}, and \\textit{R}, given}{",
    "\\textit{p_V} = $", pV,
    "$, \\textit{R}_{true} = $", R,
    "$, and \\textit{f} = $", f*100,"$%}")

  # extrafont::loadfonts(device = "win")
  plot1 = ggplot2::ggplot() +
    ggplot2::xlim(0.5, label_x + margin_offset) +

    ggplot2::expand_limits(y = 0) +
    # ggplot2::geom_function(n = n_points, size = lwd, fun = function(x) f1(x, rV = 0), xlim = c(0.5,1)) +
    # ggplot2::geom_function(n = n_points, size = lwd, fun = function(x) f1(x, rV = 0.1), xlim = c(0.5,1)) +
    ggplot2::geom_function(n = n_points, size = lwd, fun = function(x) f1(x, rV = 0.6), xlim = c(0.5,1)) +
    ggplot2::geom_function(n = n_points, size = lwd, fun = function(x) f1(x, rV = 0.7), xlim = c(0.5,1)) +
    ggplot2::geom_function(n = n_points, size = lwd, fun = function(x) f1(x, rV = 0.8), xlim = c(0.5,1)) +
    ggplot2::geom_function(n = n_points, size = lwd, fun = function(x) f1(x, rV = 0.9), xlim = c(0.5,1)) +
    ggplot2::geom_function(n = n_points, size = lwd, fun = function(x) f1(x, rV = 1.0), xlim = c(0.5,1)) +
    # geom_hline(yintercept = R, col = "black", linetype = 2) +
    ggplot2::geom_segment(ggplot2::aes(x = .5, xend = 1, y = R, yend = R), linetype = 2) +
    ggplot2::annotate("text",
             size = 6,
             family="serif",
             # angle = 30,
             parse = TRUE,
             # fill = "white",
             label = latex2exp::TeX(rv_labels, output = "character"),
             x = label_x,
             y = f1(1, rVs)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      text=ggplot2::element_text(size=16,  family="serif"),
      axis.title.x = ggplot2::element_text(size = 20),
      axis.title.y = ggplot2::element_text(angle = 0, vjust = .5, size = 20),
      axis.title.y.right = ggplot2::element_text(angle = 0, vjust = .5, size = 20),
      panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) +
    # guides(col = NULL) +
    # xlab(latex2exp::TeX("p_L = Pr(Records Linked | Event and Vaccination Both Recorded)")) +
    ggplot2::ggtitle(latex2exp::TeX(plot_title)) +
    ggplot2::xlab(latex2exp::TeX("\\textit{p_L}")) +
    ggplot2::ylab(latex2exp::TeX("\\textit{R}")) +
    # ylab(latex2exp::TeX("R^* = Estimated Relative Risk (Vaccinated/Not)")) +

    ggplot2::labs(color = latex2exp::TeX("r_V")) +
    ggplot2::scale_y_continuous(
      # limits = c(0,0.4),
      sec.axis = ggplot2::sec_axis(
        # name="Estimated Vaccine Efficacy (%)",
        name = latex2exp::TeX("\\textit{VE}"),
        labels = function(x) paste(round(x*100), "%", sep = ""),
        trans=~ (1 - .))
    ) + ggplot2::expand_limits(y = 0)

  return(plot1)
}
