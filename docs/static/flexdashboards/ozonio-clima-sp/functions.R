### Check if quotation

verify_vars <- function(call) {

  if (class(call) != "call") {
    stop("Use dplyr::vars() to select the xvars.\n")
  }

}

### Make labeller

make_labeller <- function(names) {
  
  if(is.null(names)) {
    "label_value"
  } else {
    as_labeller(names)
  }
  
}

### Ridge graphic

ridges_plot <- function(df, yvar, xvars, xlab, ylab, 
                        facets_names = NULL) {
  
  verify_vars(rlang::enexpr(xvars))
  
  yvar <- enquo(yvar)

  plot_labeller <- make_labeller(facets_names)
  
  if (length(xvars) == 1) {
    
    df %>%
      select(x = !!xvars[[1]], y = !!yvar) %>% 
      mutate(y = as.factor(y)) %>%
      ggplot(aes(x = x, y = y, fill = y)) +
      geom_density_ridges(show.legend = FALSE) +
      labs(y = ylab, x = xlab) +
      theme_bw()
    
  } else {
    
    df %>%
      mutate(y = as.factor(!!yvar)) %>% 
      select(y, !!!xvars) %>%
      tidyr::gather(serie, value, !!!xvars) %>%
      ggplot(aes(x = value, y = y, fill = y)) +
      geom_density_ridges(show.legend = FALSE) +
      labs(y = ylab, x = xlab) +
      facet_wrap(~serie, scales = "free", 
                 labeller = plot_labeller) +
      theme_bw()
    
  }
}

### Series plot

series_plot <- function(df, time_var, yvars, xlab, ylab,
                        facets_names = NULL) {
  
  verify_vars(rlang::enexpr(yvars))
  
  time_var <- enquo(time_var)
  
  plot_labeller <- make_labeller(facets_names)
  
  if (length(yvars) == 1) {
    
    df %>% 
    select(x = !!time_var, y = !!yvars[[1]]) %>% 
    ggplot(aes(x = x, y = y)) +
      geom_line() +
      geom_smooth(se = FALSE) +
      labs(x = xlab, y = ylab) +
      theme_bw()
    
  } else {
    
    df %>%
      select(x = !!time_var, !!!yvars) %>% 
      tidyr::gather(serie, value, !!!yvars) %>%
      ggplot(aes(x = x, y = value)) +
      geom_line() +
      geom_smooth(se = FALSE) +
      facet_wrap(~serie, nrow = length(yvars), 
                 scales = "free_y", labeller = plot_labeller) +
      labs(x = xlab, y = ylab) +
      theme_bw()
    
  }
}

### Scatterplot

scatter_plot <- function(df, yvar, xvars, xlab, ylab, facets_names = NULL) {
  
  verify_vars(rlang::enexpr(xvars))
  
  yvar <- enquo(yvar)

  plot_labeller <- make_labeller(facets_names)
  
  if(length(xvars) == 1) {
    
    df %>%
      select(x = !!xvars[[1]], y = !!yvar) %>% 
      ggplot(aes(x = x, y = y)) +
      geom_point() +
      geom_smooth(se = FALSE) +
      labs(x = xlab, y = ylab) +
      theme_bw()
    
  } else {
    df %>%
      select(y = !!yvar, !!!xvars) %>% 
      tidyr::gather(serie, value, !!!xvars) %>%
      ggplot(aes(x = value, y = y)) +
      geom_point() +
      geom_smooth(se = FALSE) +
      facet_wrap(~serie, scales = "free", labeller = plot_labeller) +
      labs(x = xlab, y = ylab) +
      theme_bw()
  }
}


