tvp_theme <- function(base_size = 17L, no_grid = FALSE, manuscript_mode = FALSE){
  if (manuscript_mode == TRUE) { # Omit all titles, subtitles and captions
    if(no_grid == FALSE){
    ggplot2::theme_bw(base_size = base_size) +
      ggplot2::theme(
        text = element_text(color = "gray45")
        , axis.text            = element_text(size = rel(1))
        , plot.title           = element_blank()
        , plot.subtitle        = element_blank()
        , plot.caption         = element_blank()
        , legend.title         = element_blank()
        , panel.border         = element_blank()
        , axis.line            = element_line(color = 'black')
        , legend.position      = 'bottom'
        , legend.justification = 'left'
        , panel.grid           = element_line(color = 'gray92')
        , strip.text = element_text(color = "white")
        , strip.background     = element_rect(fill = "gray92",
                                            color = "gray92")
      )
    }
    if (no_grid == TRUE) {
      ggplot2::theme_bw(base_size = base_size) +
        ggplot2::theme(
          text = element_text(color = "gray45")
          , axis.text            = element_text(size = rel(1))
          , plot.title           = element_blank()
          , plot.subtitle        = element_blank()
          , plot.caption         = element_blank()
          , legend.title         = element_blank()
          , panel.border         = element_blank()
          , axis.line            = element_line(color = 'black')
          , legend.position      = 'bottom'
          , legend.justification = 'left'
          , panel.grid           = element_line(color = 'gray92')
          , strip.text = element_text(color = "white")
          , strip.background     = element_rect(fill = "gray92",
                                                color = "gray92")
        ) + ggplot2::theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
    }
  }
  if (manuscript_mode == FALSE) {
    if(no_grid == FALSE){
    ggplot2::theme_bw(base_size = base_size) +
      ggplot2::theme(
        text = element_text(color = "gray45")
        , axis.text            = element_text(size = rel(1))
        , plot.title           = element_text(size = rel(1.6)
                                              , color = "gray1"
                                              , family = "serif")
        , plot.subtitle        = element_text(size = rel(1.2))
        , legend.title         = element_blank()
        , panel.border         = element_blank()
        , axis.line            = element_line(color = 'black')
        , legend.position      = 'bottom'
        , legend.justification = 'left'
        , panel.grid           = element_line(color = 'gray92')
        , strip.text           = element_text(color = "white")
        , strip.background     = element_rect(fill = "gray92",
                                              color = "gray92")
      ) 
      }
    if (no_grid == TRUE) {
      ggplot2::theme_bw(base_size = base_size) +
        ggplot2::theme(
          text = element_text(color = "gray45")
          , axis.text            = element_text(size = rel(1))
          , plot.title           = element_text(size = rel(1.6)
                                                , color = "gray1"
                                                , family = "serif")
          , plot.subtitle        = element_text(size = rel(1.2))
          , legend.title         = element_blank()
          , panel.border         = element_blank()
          , axis.line            = element_line(color = 'black')
          , legend.position      = 'bottom'
          , legend.justification = 'left'
          , panel.grid           = element_line(color = 'gray92')
          , strip.text           = element_text(color = "white")
          , strip.background     = element_rect(fill = "gray92",
                                                color = "gray92")
        ) + ggplot2::theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
    }
  }
}
