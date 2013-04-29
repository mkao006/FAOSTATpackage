##' A plot to show the availability of the data
##'
##' This function is based on ggmissing of the ggplot2 package with
##' minor modification.
##'
##' @param data The data frame to examine the sparsity
##' @param order Whether the plot should reorder the data by the sparsity
##' @param threshold The level where the horizontal line to be added.
##' @export
dataDensity = function (data, order = TRUE, threshold = 0.5){
    missings <- mapply(function(var, name)
                       cbind(as.data.frame(table(Available = factor(!is.na(var),
                                                 levels = c(TRUE, FALSE),
                                                 labels = c("yes", "no")))),
                             variable = name), data, names(data),
                       SIMPLIFY = FALSE)
    df = do.call("rbind", missings)
    prop = df[df$Available == "yes", "Freq"]/(df[df$Available ==
        "no", "Freq"] + df[df$Available == "yes", "Freq"])
    df[as.logical((1:NROW(df)) %% 2), "prop"] = prop
    df[!((1:NROW(df)) %% 2), "prop"] = 1 - prop
    if (order) {
        var = df$variable
        var = factor(var, levels = levels(var)[order(1 - prop)])
        df$variable = var
    }
    ggplot(df, aes_string(y = "prop", x = "variable", fill = "Available")) +
        geom_bar(position = "stack", stat = "identity") +
        geom_abline(intercept = threshold, slope = 0,
                    linetype = "dashed") +
        scale_fill_manual(values = c("#4477AA", "#CC6677")) +
        scale_y_continuous(labels = percent) +
        ylab("Percentage of available data") + xlab(NULL) +
        theme(legend.position = "top", legend.direction = "horizontal")
}

