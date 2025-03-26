#' @title generate column names.
#' @description \code{rename_cols} The column names of the matrix of category response probabilities obtained by calculate_irccc are used as the rating categories. If the rating category is 10 or more, the tens place of the column names of the rating categories of 9 or less is padded with zeros. For example, if the rating category is 7, it becomes grade07.
#'
#' @param k numeric. Number of rating categories for any given item. It must be 2 or more.
#' @keywords internal
rename_cols <- function(k) {
  if (k < 2) {
    stop("The number of rating categories must be 2 or more. This package does not yet support anything other than GRM.")
  }
  if (k < 10) {# k < 10
    col_names <- paste0("grade", 1:k)
  } else {# k >= 10
    col_names <- sprintf("grade%02d", 1:k)
  }
  return(col_names)
}

#' @title A category response probability is calculated from the item parameter.
#' @description \code{calculate_irccc} Calculate a category response probability from the item parameter.
#'
#' @param thetas vector. Theta is the latent value of the range for calculating the category response probability. This value is calculated from the theta and breaks of gg_irccc.
#' @param a numeric. The length must be 1. This represents the identification parameter.
#' @param b vector. This is the parameter for difficulty level.
#' @param item numeric. The length must be 1. This value is the number of the item for calculating the category response probability.
#' @param use_ltm logical. The default is FALSE. If you are using the mirt object, set this to FALSE. If you are using the grm object from the ltm package, set this option to TRUE. When use_ltm = TRUE, the scale factor D is corrected to 1.701.
#' @param k numeric. Number of rating categories for any given item. It must be 3 or more.
#' @keywords internal
calculate_irccc <- function(thetas, a, b, item, k, use_ltm) {
  # cumulative probability
  ai = a[item]
  bi = b[item, ]
  if(use_ltm == TRUE){
    p_cum = sapply(bi, function(bi) 1 / (1 + exp(-(1.701*ai) * (thetas - bi))))
  }
  if(use_ltm == FALSE){
    p_cum = sapply(bi, function(bi) 1 / (1 + exp(-ai * (thetas - bi))))
  }  # add boundary condition (0 and 1)
  p_cum = cbind(1, p_cum, 0)
  p_cum_rev = p_cum[, ncol(p_cum):1]
  # calculate the probability of adjacency
  probs = apply(p_cum_rev, 1, diff)
  probs = probs[, ncol(probs):1]
  probs = t(probs)
  # rename col. names with rename_cols function.
  col_name = rename_cols(k + 1)
  colnames(probs) = col_name
  return(probs)
}

#' @title Extracts item parameters from the mirt object.
#' @description \code{get_par} Extracts item parameters (a and bs) from the mirt object.
#'
#' @importFrom mirt mirt
#' @importFrom mirt coef
#' @param object mirt object
#' @keywords internal
get_par <- function(object){
  data <- mirt::coef(object, simplify = TRUE, IRTpars = TRUE)
  data <- data$items
  return(data)
}

#' @title visualize IRCCC from mirt or ltm object.
#' @description \code{gg_irccc} visualize IRCCC from mirt or ltm object.
#'
#' @importFrom mirt mirt
#' @importFrom ltm grm
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 scale_linetype_manual
#' @param object mirt or grm object. If you use the grm object, you must set ltm = TRUE.
#' @param item numeric. The number of the item to be visualized. The length must be 1.
#' @param theta vector. The length must be 2. Specify the range of theta in the IRCCC to be visualized using c().
#' @param breaks numeric. Specifies the number of divisions of theta. For example, if you specify 1000, the range specified by theta will be divided into 1000 parts. The default is 100.
#' @param grm logical. The default is TRUE. If the object is GRM, set it to TRUE, and if it is a binary type, set it to FALSE.
#' @param monochrome logical. The default is FALSE, and IRCCC is output in color. If TRUE, IRCCC is output in black and white.
#' @param use_ltm logical. The default is FALSE. If you are using the mirt object, set this to FALSE. If you are using the grm object from the ltm package, set this option to TRUE. When use_ltm = TRUE, the scale factor D is corrected to 1.701.
#' @param output.data logical. The default is FALSE. If TRUE, instead of visualizing IRCCC, the data that forms the basis of IRCCC is output in long format.
#' @return Visualize the IRCCC of the specified item from the mirt or ltm object (output.data = FALSE). Output data that can visualize the IRCCC of the specified item from the mirt or ltm object (output.data = TRUE).
#' @export
#' @examples
#' #library(mirt)
#' #data(Bock1997)
#' #dat <- Bock1997[1:3]
#' #res <- mirt(dat, model = 1, itemtype = "graded")
#'
#' #gg_irccc(object = res, item = 1)
#' #plot1 <- gg_irccc(object = res, item = 1)
#' #plot1 + theme_bw()
#'
#' #library(ltm)
#' #res <- grm(dat[1:3], IRT.param = TRUE)
#'
#' #gg_irccc(object = res, item = 1, use_ltm = TRUE)
#' #d <- gg_irccc(object = res, item = 1, output.data = TRUE)
gg_irccc <- function(object, item, theta = c(-3, 3), breaks = 100, grm = TRUE, monochrome = FALSE,
                     use_ltm = FALSE, output.data = FALSE) {
  # check object class
  if(use_ltm == FALSE) {
    if(!inherits(object, "SingleGroupClass")) {
      stop("The object must be mirt object.")
    }
    if(inherits(object, "MultipleGroupClass")) {
      stop("This package does not yet support MultipleGroupClass.")
    }}
  if(use_ltm == TRUE && !inherits(object, c("ltm", "grm"))) {
    stop("When ltm is TRUE, the object must be an ltm or grm object.")
  }
  # check theta values
  if(theta[1] == theta[2]) {
    stop("`theta` values are same. They must be two different values.")
  }

  # check item value
  if (is.numeric(item) != TRUE) {
    stop("`item` must be numeric.")
  }
  if (length(item) != 1) {
    stop("`item` must be a number with length 1. This package does not yet support multiple plots.")
  }

  # check monochrome value
  if (is.logical(monochrome) != TRUE) {
    stop("`monochrome` must be TRUE or FALSE.")
  }

  # check output.data value
  if (is.logical(output.data) != TRUE) {
    stop("`output.data` must be TRUE or FALSE.")
  }

  # check grm value
  if (is.logical(grm) != TRUE) {
    stop("`grm` must be TRUE or FALSE.")
  }

  # check argument values part above
  if (grm == TRUE) {# item type is GRM.
    if(use_ltm == FALSE) {
      data = get_par(object)
      item = item
      k = ncol(data) - 1# number of rating categories
      a = data[, 1]# discrimination power
      b = data[, 2:ncol(data)]# difficulties
      b = round(b, digits = 2)
    }
    if(use_ltm == TRUE) {
      data = coef(object)
      item = item
      k = ncol(data) - 1
      tail_matrix = k + 1
      a = data[, tail_matrix]
      b = data[, 1:k]
      b = round(b, digits = 2)
    }
  }

  if (grm == FALSE) {
    if(use_ltm == FALSE) {
      data = get_par(object)
      item = item
      k = 2# number of rating categories
      a = data[, 1]# discrimination power
      b = data[, 2:ncol(data)]# difficulties
      b = round(b, digits = 2)
    }
    if(use_ltm == TRUE) {
      data = coef(object)
      item = item
      k = 2
      a = data[, 2]
      b = data[, 1]
      b = round(b, digits = 2)
    }
  }

  thetas = seq(min(theta), max(theta), length.out = breaks)

  probs = calculate_irccc(thetas = thetas, a = a, b = b, item = item, use_ltm = use_ltm, k = k)#calculate irccc
  n_theta = length(thetas)
  times = k+1
  col_names = rename_cols(k = times)
  # dataset as data.frame
  if (grm == TRUE) {#GRM
    plot_data =
      data.frame(
        Theta = rep(thetas, times = times),
        Probability = as.vector(probs),
        Grade = factor(rep(col_names, each = n_theta))
      )
  }
  if (grm == FALSE) {#2PLM
    plot_data =
      data.frame(
        Theta = rep(thetas, times = times),
        Probability = as.vector(probs)
      )
  }


  if (output.data == FALSE){
    if (grm == TRUE) {
      if (monochrome) {
        gg = ggplot(plot_data, aes(x = Theta, y = Probability, linetype = Grade)) + geom_line(size = 1)
      } else {
        gg = ggplot(plot_data, aes(x = Theta, y = Probability, linetype = Grade)) + geom_line(aes(colour = Grade), size = 1)
      }
      gg = gg + ylim(c(0.00, 1.00)) + labs(title = paste("item", item)) +
        theme_minimal(base_family = "Times", base_size = 12) +    #Font Size Conforming to APA Standards
        theme(
          axis.text = element_text(size = 10, color = "black"),   # Axis labels
          axis.title = element_text(size = 12),                   # Axis title
          panel.grid.major = element_line(size = 0.5, linetype = "solid", color = "gray80"),  # grid lines
          panel.grid.minor = element_blank(),                     # Hide minor grid lines
          panel.border = element_blank(),                         # Remove panel border lines
          axis.line = element_line(size = 0.8, color = "black"),  # Emphasize the axis
          legend.text = element_text(size = 12)                   # font size in legend
        )
      return(gg)}
    if (grm == FALSE) {
      gg = ggplot(plot_data, aes(x = Theta, y = Probability)) + geom_line(size = 1)
      gg = gg + ylim(c(0.00, 1.00)) + labs(title = paste("item", item)) +
        theme_minimal(base_family = "Times", base_size = 12) +    #Font Size Conforming to APA Standards
        theme(
          axis.text = element_text(size = 10, color = "black"),   # Axis labels
          axis.title = element_text(size = 12),                   # Axis title
          panel.grid.major = element_line(size = 0.5, linetype = "solid", color = "gray80"),  # grid lines
          panel.grid.minor = element_blank(),                     # Hide minor grid lines
          panel.border = element_blank(),                         # Remove panel border lines
          axis.line = element_line(size = 0.8, color = "black"),  # Emphasize the axis
          legend.text = element_text(size = 12)                   # font size in legend
        )
    }
  }

  if (output.data == TRUE) {
    return(plot_data)
  }
}
