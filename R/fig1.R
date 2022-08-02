#' Title
#'
#' @param pV
#' @param R
#' @param f
#' @param n_points
#' @param label_x
#' @param lwd
#' @param margin_offset
#' @param add_title
#' @param rv_size
#' @param top_border
#' @param text_size
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
    margin_offset = .02,
    add_title = TRUE,
    rv_size = 4.5,
    text_size = 16,
    axis_title_size = 20,
    axis_tick_text_size = 12,
    top_border = TRUE,
    y_limits = 0
)
{
  f1 = function(pL, rV)
  {
    reporting_multiplier(
      f = f,
      `p(V)` = pV,
      R = R,
      `p(V*|V)` = rV,
      `p(L|V*,E*)` = pL) %>% pull(`R*`)

  }

  rVs = c(
    # 0, .1,
    .6, .7, .8, .9, 1)
  rv_labels = paste(r"($\textit{r_V} =$)", formatC(rVs, format = "f", digits = 1 ))
  plot_title = paste0(
    r"(\overset{Relationship between $\textit{p_L}$, $\textit{r_V}$, and $\textit{R}$, given}{)",
    r"($\textit{p_V} = )", pV,
    r"($, $\textit{R_{true}} = )", R,
    r"($, and $\textit{f} = )", f*100, r"($%})")

  # extrafont::loadfonts(device = "win")
  plot1 = ggplot2::ggplot() +
    ggplot2::xlim(0.5, label_x + margin_offset) +

    ggplot2::expand_limits(y = 0) +
    # ggplot2::geom_function(n = n_points, size = lwd, fun = function(x) f1(x, rV = 0), xlim = c(0.5,1)) +
    # ggplot2::geom_function(n = n_points, size = lwd, fun = function(x) f1(x, rV = 0.1), xlim = c(0.5,1)) +
    ggplot2::geom_segment(ggplot2::aes(x = .5, xend = 1, y = R, yend = R), linetype = 2) +

    ggplot2::geom_function(n = n_points, size = lwd, fun = function(x) f1(x, rV = 0.6), xlim = c(0.5,1)) +
    ggplot2::geom_function(n = n_points, size = lwd, fun = function(x) f1(x, rV = 0.7), xlim = c(0.5,1)) +
    ggplot2::geom_function(n = n_points, size = lwd, fun = function(x) f1(x, rV = 0.8), xlim = c(0.5,1)) +
    ggplot2::geom_function(n = n_points, size = lwd, fun = function(x) f1(x, rV = 0.9), xlim = c(0.5,1)) +
    ggplot2::geom_function(n = n_points, size = lwd, fun = function(x) f1(x, rV = 1.0), xlim = c(0.5,1)) +
    # geom_hline(yintercept = R, col = "black", linetype = 2) +
    ggplot2::annotate("text",
                      size = rv_size,
                      family="Times",
                      # angle = 30,
                      parse = TRUE,
                      # fill = "white",
                      label = latex2exp::TeX(rv_labels, output = "character"),
                      x = label_x,
                      y = f1(1, rVs)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      text = ggplot2::element_text(size= text_size,  family="Times"),
      axis.text.x = ggplot2::element_text(size = axis_tick_text_size),
      axis.text.y = ggplot2::element_text(size = axis_tick_text_size),
      axis.text.y.right = ggplot2::element_text(size = axis_tick_text_size),
      axis.title.x = ggplot2::element_text(size = axis_title_size),
      axis.title.y = ggplot2::element_text(
        # angle = 0,
        vjust = .5,
        size = axis_title_size),
      axis.title.y.right = ggplot2::element_text(
        # angle = 0,
        vjust = .5, size = axis_title_size),
      panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) +
    # guides(col = NULL) +
    # xlab(latex2exp::TeX("p_L = Pr(Records Linked | Event and Vaccination Both Recorded)")) +
    ggplot2::xlab(latex2exp::TeX(r'($\textit{p_L}$)')) +
    ggplot2::ylab(latex2exp::TeX(r'($R$)')) +
    # ylab(latex2exp::TeX("R^* = Estimated Relative Risk (Vaccinated/Not)")) +

    ggplot2::labs(
      linetype = latex2exp::TeX(r'($\textit{r_V}$)'),
      color = latex2exp::TeX(r'($\textit{r_V}$)')) +

    ggplot2::scale_y_continuous(
      # limits = c(0,0.4),
      sec.axis = ggplot2::sec_axis(
        # name="Estimated Vaccine Effectiveness (%)",
        name = latex2exp::TeX(r'($VE,$ %)'),
        labels = function(x) paste(round(x*100), #"%",
                                   sep = ""),
        trans=~ (1 - .))
    ) +
    ggplot2::expand_limits(y = y_limits) +
    theme(legend.position="none",
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20))

  if(add_title)
  {
    plot1 = plot1 + ggplot2::ggtitle(latex2exp::TeX(plot_title))
  }

  if(!top_border)
  {
    plot1 =
      plot1 +
      theme(
        panel.border = element_blank(),
        axis.line = element_line(size = .2))
  }

  return(plot1)
}
