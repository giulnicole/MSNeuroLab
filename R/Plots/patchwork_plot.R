
### Patchwork plot 


# Combining plots into pre-defined sequences (1 window)

if(!require('patchwork')) {
  install.packages('patchwork')
  library('patchwork')
}

library(ggplot2)


p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
p3 <- ggplot(mtcars) + geom_bar(aes(gear)) + facet_wrap(~cyl)
p4 <- ggplot(mtcars) + geom_bar(aes(carb))
p5 <- ggplot(mtcars) + geom_violin(aes(cyl, mpg, group = cyl))



# Either add the plots as single arguments
#wrap_plots(p1, p2, p3, p4, p5)


# Or add them as a list
plots <- list(p1, p2, p3, p4, p5)
#wrap_plots(plots)


# Match plots to areas by name
design <- "#BBBBCCCCCCCCCF
           #DDDDDDDDDDDDD#
           #####AAA##EEE##"
wrap_plots(B = p1, A = p2, C = p3, D = p4, E = p5, F = p1, design = design, axis_titles = "collect") + plot_annotation(tag_levels = list(c('A', '', 'B', '', 'C', 'D'))) & theme(plot.tag = element_text(size = 16, face = "bold", color = "blue"))

