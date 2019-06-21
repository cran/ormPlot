#' @rdname forestplot
#' @param ... see parameters of method \code{\link{forestplot}}
#' @method plot summary.rms
#' @export
"plot.summary.rms"<-function(x, ...){
    UseMethod("forestplot")
}


#' Forest Plot of an rms model summary
#'
#' Convenience function to create a plot of the \code{\link[rms]{orm}}  model
#' summary. For further customizing the plots use \code{return_ggplots = TRUE}
#' This will create 2 \code{ggplot2} objects that can be joined with the
#' \code{\link{join_ggplots}} commands.
#'
#' @param return_ggplots if \code{TRUE} the function returns 2 ggplot objects
#'  in a list instead of drawing a tablegrid
#' @param  x result of a \code{summary} command on
#'  \code{\link[rms]{orm}} or \code{\link[rms]{lrm}}  model ie a
#'  \code{\link[rms]{summary.rms}} class object
#' @inheritParams join_ggplots
#' @inheritParams oddstable_graph
#' @inheritParams orm_graph
#' @aliases forestplot.orm forestplot.lrm
#' @example inst/examples/forestplot.R
#' @export
forestplot <- function(x, return_ggplots = FALSE,
                               plot.widths = c(0.5,0.5),
                               title = "Odds ratio",
                               digits = 3,
                               shape = 19,
                               header = NULL,
                               limits = NULL,
                               breaks = c(0.5, 1, 1.5, 2, 3, 4),
                               theme = ggplot2::theme_get(),
                               row.names.y = NULL) {

    if(!inherits(x,"summary.rms"))  {
        if(inherits(x,"orm") | inherits(x,"lrm")){
            stop(paste0("The method must be run on a summary.rms object. ",
                        "Use function summary(x",
                        ") to create it!", collapse = ""))
        }
        else {
            stop(paste0("The method must be run on a summary.rms object ",
                        "that is created from a rms model object!",
                         collapse = ""))
        }
    }

    oddstable <- oddstable(x)

    tableplot <- oddstable_graph(oddstable, digits, theme,
                                 header, row.names.y)


    tablegraph <- orm_graph(oddstable, theme, header,
                            row.names.y, shape, limits,
                            breaks)

    if (return_ggplots) {
        return(list(tableplot, tablegraph))

    } else {
        forestplot <- join_ggplots(tableplot, tablegraph, plot.widths, title)
        invisible(forestplot)
    }



}
#' @rdname forestplot
#' @export
forestplot.default <- function(x, ...){
    warning(paste("ormPlot does not know how to handle object of class ",
                  class(x),
                  "and can only be used on summary.rms"))
}

#' @rdname forestplot
#' @export
forestplot.summary.rms <- function(x, ...){
    forestplot(x, ...)
}

#' Join two ggplot objects side by side
#'
#' Function to get aligned table of two ggplot objects
#'
#'
#' @param leftplot the left side plot
#' @param rightplot the plot on the right
#' @param plot.widths the relative widths of the left and right plot
#' should be a vector (\code{c()})  with 2 elements that sum to 1 defaults to
#' equal widths
#' @param title the tile row of the drawn plot
#' @example inst/examples/join_ggplots.R
#' @export
join_ggplots <- function(leftplot, rightplot,
                         plot.widths = c(0.5, 0.5),
                         title = "Odds Ratio") {
    if (length(plot.widths) != 2 || signif(sum(plot.widths), 3) != 1)
        stop("plot.widths should be a vector with 2 elements that sum to 1")

    jointheme <-  ggplot2::theme(plot.margin = grid::unit(c(0, 0, 0, 0), "lines"),
                                 plot.subtitle = ggplot2::theme_get()$text)


    tablewidth <- grid::unit(c(plot.widths[1], plot.widths[2]), c("npc"))
    p1g <- ggplot2::ggplotGrob(leftplot +
                                ggplot2::labs(subtitle = "") +
                                jointheme
                               )
    p2g <- ggplot2::ggplotGrob(rightplot +
                               ggplot2::labs(subtitle = title) +
                               jointheme)
    forestplot <- gtable::gtable_row("forestplot", list(p1g, p2g),
                                     widths = tablewidth,
                                     height = grid::unit(1, "npc"))
    grid::grid.newpage()
    grid::grid.draw(forestplot)
    invisible(forestplot)
}
#' Get row names from odd an values form even columns
#' @param x a \code{matrix} with even number of rows
oddstable <- function(x) {
        if (nrow(x) %% 2 == 0) {
            cstats <- x[c(FALSE, TRUE), c(1:ncol(x))]
            dimnames(cstats) <- list(dimnames(x[c(TRUE, FALSE), ])[[1]],
                                     dimnames(x)[[2]][1:ncol(x)])
            invisible(cstats)
        } else {
            stop("Aborting! The number of input rows is odd!")
        }

}

#' Make a ggplot table
#'
#' Function to get a ggplot table from a matrix
#'
#' @param x a \code{matrix} or a \code{data.frame}
#' @param digits the number of significant digits to display
#' @param theme the desired ggplot2 theme
#' @param header names of the table columns
#' @param row.names.y new names for the variable rows
#'
oddstable_graph <- function(x, digits = 3, theme = ggplot2::theme_get(),
                           header = NULL, row.names.y = NULL) {
    columns <- c(4, 6, 7)
    #see https://stackoverflow.com/questions/17311917/ggplot2-the-unit-of-size
    text_size_adjust <- (1 / 72.27) * 25.4
    if (is.null(row.names.y))
        row.names.y <- rownames(x)

    if (is.vector(header) && length(header) == length(columns)) {
        # keep the provided header
    } else {
        header <- colnames(x[, columns])
        #rename the first
        header[1] <- "Est."
    }
    # setting the theme from the function
    ggplot2::theme_set(theme)

    if (!is.data.frame(x))
        x <- as.data.frame(x)

    tableplot <- ggplot2::ggplot(x, ggplot2::aes(y = rownames(x))) +
        ggplot2::scale_x_discrete(position = "top",
                                  name = NULL,
                                  labels = header,
                                  breaks = colnames(x[, columns]))

    tableplot <- tableplot +
        ggplot2::geom_text(ggplot2::aes(x = colnames(x)[4],
                                        label = round(x[, 4], digits)),
                           color = theme$text$colour,
                           size =  theme$text$size*text_size_adjust,
                           family = theme$text$family
                           ) +
        ggplot2::geom_text(ggplot2::aes(x = colnames(x)[6],
                                        label = round(x[, 6], digits)),
                            color=theme$text$colour,
                           size =  theme$text$size*text_size_adjust,
                            family = theme$text$family) +
        ggplot2::geom_text(ggplot2::aes(x = colnames(x)[7],
                                        label = round(x[, 7], digits)),
                           color=theme$text$colour,
                           size =  theme$text$size*text_size_adjust,
                           family = theme$text$family)

    tableplot <- tableplot +
        ggplot2::scale_y_discrete(limits = rev(rownames(x)),
                                  labels = rev(row.names.y)) +
        ggplot2::theme(axis.ticks = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(colour = "black"),
        axis.title.y = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(hjust = 1),
        axis.text = theme$text,
        plot.margin = grid::unit(c(0, 0, 0, 0), "lines"))


    invisible(tableplot)

}
#' Make a ggplot figure
#'
#' Function to get a ggplot figure from a matrix x
#'
#' @param x a \code{matrix} or a \code{data.frame}
#' @param theme the desired ggplot2 theme
#' @param header names of the table columns
#' @param row.names.y new names for the variable rows
#' @param shape point shape, see \code{\link[ggplot2]{aes_linetype_size_shape}}
#' @param limits the x axis limits as a vector, see also:
#'  \code{\link[ggplot2]{scale_continuous}}
#' @param breaks the x axis breaks as a vector,help see also:
#'  \code{\link[ggplot2]{scale_continuous}}
#'
orm_graph <- function(x, theme = ggplot2::theme_get(), header = NULL,
                           row.names.y = NULL, shape = 19, limits = NULL,
                           breaks = c(0.5, 1, 1.5, 2, 3, 4)) {
    # set the theme
    ggplot2::theme_set(theme)

    if (!is.data.frame(x))
        x <- as.data.frame(x)

    if (is.null(row.names.y))
        row.names.y <- rownames(x)


    if (!length(row.names.y) == nrow(x)) {
        vars <- paste(rownames(x), collapse = "\", \"")
        stop(paste("The provided variable names are not covering all options: ",
                   vars))
    }
    if(is.null(header)){
        header <- c(colnames(x)[4], colnames(x)[6], colnames(x)[7])
    }

    colnames(x)<-make.names(colnames(x))
    x$names <- row.names.y
    effect_val <- as.character(colnames(x)[4])
    lower_val <- as.character(colnames(x)[6])
    upper_val <- as.character(colnames(x)[7])
    p <- ggplot2::ggplot(x, ggplot2::aes_string(x = "names",
                                                y = eval(effect_val),
                                         ymin = eval(lower_val),
                                         ymax = eval(upper_val))) +
        ggplot2::geom_pointrange(colour = theme$line$colour, shape = shape) +
    # add a dotted line at x=1 after flip
    ggplot2::geom_hline(yintercept = 1, lty = 2) +
    # flip coordinates (puts labels on y axis)
    ggplot2::coord_flip() +
    # TODO set the breaks at appropriate places automatically and allow override
    ggplot2::scale_y_continuous(breaks = breaks,
                                position = "right", limits = limits) +
    # set the y lables sam for plot and table
    ggplot2::scale_x_discrete(limits = rev(row.names.y)) +
    # use the theme set prevoiosly and modify some things
    ggplot2::theme(axis.ticks.x.top = ggplot2::element_line(colour = "black"),
                   axis.line = ggplot2::element_line(colour = "black"),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   text = ggplot2::element_text(size = 12),
                   axis.text = theme$text,
                   plot.margin = grid::unit(c(0, 0, 0, 0), "lines"))



    invisible(p)
}
