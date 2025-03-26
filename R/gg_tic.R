#' @title calculate item information from GRM.
#' @description \code{item_info_grm} Calculate the item information volume from the item parameter obtained from GRM.
#'
#' @param thetas vector.
#' @param a vector. Discriminating power.
#' @param b matrix. Difficulty level.
#' @param item numeric. Item number
#' @param use_ltm logical. The default is FALSE. If you are using an object (either an ltm object or a grm object) from the ltm package, set this to TRUE.
#' @keywords internal
item_info_grm <- function(thetas, a, b, item, use_ltm) {
  K <- ncol(b)  # number of category response
  ai = a[item]
  bi = b[item, ]
  n_theta <- length(thetas)

  if (use_ltm == FALSE) {
    p_star <- sapply(b, function(bk) 1 / (1 + exp(-ai * (thetas - bk))))
  }

  if (use_ltm == TRUE) {
    p_star <- sapply(b, function(bk) 1 / (1 + exp(-(1.701*ai) * (thetas - bk))))
  }

  p_star <- cbind(1, p_star, 0)

  p_cat <- p_star[, 1:K] - p_star[, 2:(K+1)]

  info <- numeric(n_theta)
  for (i in 1:n_theta) {
    sum_info <- 0
    for (j in 1:K) {
      Pk_star <- p_star[i, j]
      Pk1_star <- p_star[i, j+1]
      Pk <- p_cat[i, j]
      numerator <- (Pk_star * (1 - Pk_star) - Pk1_star * (1 - Pk1_star))^2
      if (Pk > 0) {
        sum_info <- sum_info + numerator / Pk
      }
    }

    if (use_ltm == FALSE) {
      info[i] <- ai^2 * sum_info
    }
    if (use_ltm == TRUE) {
      info[i] <- (1.701*ai)^2 * sum_info
    }
  }

  return(info)
}

#' @title calculate item information from 2PLM.
#' @description \code{item_info_2plm} Calculate the item information volume from the item parameter obtained from 2PLM.
#'
#' @param thetas vector.
#' @param a vector. Discriminating power.
#' @param b matrix. Difficulty level.
#' @param item numeric. Item number
#' @param use_ltm logical. The default is FALSE. If you are using an object (either an ltm object or a grm object) from the ltm package, set this to TRUE.
#' @keywords internal
item_info_2plm <- function(thetas, a, b, item, use_ltm) {
  ai = a[item]
  bi = b[item]

  if (use_ltm == FALSE){
    P = 1 / (1 + exp(-ai * (thetas - bi)))
  }

  if (use_ltm == TRUE){
    P = 1 / (1 + exp(-(1.701*ai) * (thetas - bi)))
  }

  info = ai^2 * P * (1 - P)

  return(info)
}

#' @title calculate item information.
#' @description \code{calculate_iic} Calculate the item information volume from the item parameter obtained from GRM or 2PLM.
#'
#' @param object mirt or grm object. If you use the grm object, you must set ltm = TRUE.
#' @param item numeric. The number of the item to be visualized. The length must be 1.
#' @param theta vector. The length must be 2. Specify the range of θ in the IRCCC to be visualized using c().
#' @param breaks numeric. Specifies the number of divisions of theta. For example, if you specify 1000, the range specified by theta will be divided into 1000 parts. The default is 100.
#' @param grm logical. The default is TRUE. The default is TRUE. If you are using a binary IRT　(2PLM) object, set it to TRUE.
#' @param use_ltm logical. The default is FALSE. If you are using an object (either an ltm object or a grm object) from the ltm package, set this to TRUE.
#' @keywords internal
calculate_iic <- function(object, item, theta, breaks, grm, use_ltm) {
  thetas = seq(min(theta), max(theta), length.out = breaks)

  if (use_ltm == FALSE) {
    data = get_par(object)
    item = item
    k = ncol(data) - 1# number of rating categories
    a = data[, 1]# discrimination power
    b = data[, 2:ncol(data)]# difficulties
    b = round(b, digits = 2)
  } else {
    data = coef(object)
    item = item
    k = ncol(data) - 1
    tail_matrix = k + 1
    a = data[, tail_matrix]
    b = data[, 1:k]
    b = round(b, digits = 2)
  }

  if (grm == TRUE) {
    item_info = item_info_grm(thetas = thetas, a = a, b = b, item = item, use_ltm = use_ltm)
  }

  if (grm == FALSE) {
    item_info = item_info_2plm(thetas = thetas, a = a, b = b, item = item, use_ltm = use_ltm)
  }

  return(item_info)
}

#' @title visualize Item Information Curves.
#' @description \code{gg_iic} Calculate the item information volume from the item parameter obtained from GRM or 2PLM.
#'
#' @param object mirt or grm object. If you use the grm object, you must set ltm = TRUE.
#' @param item numeric. The number of the item to be visualized. The length must be 1.
#' @param theta vector. The length must be 2. Specify the range of θ in the IRCCC to be visualized using c().
#' @param breaks numeric. Specifies the number of divisions of theta. For example, if you specify 1000, the range specified by theta will be divided into 1000 parts. The default is 100.
#' @param grm logical. The default is TRUE. If you are using a binary IRT　(2PLM) object, set it to TRUE.
#' @param use_ltm logical. The default is FALSE. If you are using an object (either an ltm object or a grm object) from the ltm package, set this to TRUE.
#' @param output.data Logical. The default is FALSE. If TRUE, instead of visualizing IRCCC, the data that forms the basis of IRCCC is output in long format.
#' @return Visualize item information using ggplot2 package.
#' @export
#' @examples
#' #library(mirt)
#' #mirt_object <- mirt(data, itemtype = "graded")
#'
#' #gg_iic(mirt_object, item = 1)
#' #gg <- gg_iic(mirt_object, item = 1)
#' #gg + theme_bw()
#'
#' #library(ltm)
#' #ltm_object <- ltm(data ~ z1, IRT.param = TRUE)
#'
#' #gg_iic(ltm_object, item = 1, theta = c(-5, 5), breaks = 1000, grm = FALSE, ltm = TRUE)
gg_iic <- function(object, item, theta = c(-3, 3), breaks = 100, grm = TRUE, use_ltm = FALSE, output.data = FALSE) {
  # check arguments part below
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


  # check output.data value
  if (is.logical(output.data) != TRUE) {
    stop("`output.data` must be TRUE or FALSE.")
  }

  # check grm value
  if (is.logical(grm) != TRUE) {
    stop("`grm` must be TRUE or FALSE.")
  }
  # check arguments part above

  # calculate item information
  item_info = calculate_iic(object, item, theta, breaks, grm, use_ltm)
  thetas = seq(min(theta), max(theta), length.out = breaks)

  plot_data = data.frame(
    Theta = thetas,
    ItemInfo = item_info
  )

  gg = ggplot(data = plot_data, aes(x = Theta, y = ItemInfo)) + geom_line(size = 1)
  gg = gg + labs(title = paste("IIC for item", item),
                 x = "Theta", y = "Item Information") +
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

  if(output.data == TRUE) {
    return(plot_data)
  }

  return(gg)
}

#' @title calculate test information.
#' @description \code{calculate_test_info} Calculate the test information volume from the item parameter obtained from GRM or 2PLM.
#'
#' @param object mirt or grm object. If you use the grm object, you must set use_ltm = TRUE.
#' @param theta vector. The length must be 2. Specify the range of θ in the IRCCC to be visualized using c().
#' @param breaks numeric. Specifies the number of divisions of theta. For example, if you specify 1000, the range specified by theta will be divided into 1000 parts. The default is 100.
#' @param grm logical. The default is TRUE. The default is TRUE. If you are using a binary IRT　(2PLM) object, set it to TRUE.
#' @param use_ltm logical. The default is FALSE. If you are using an object (either an ltm object or a grm object) from the ltm package, set this to TRUE.
#' @keywords internal
calculate_test_info <- function(object, theta, breaks, grm, use_ltm) {
  thetas = seq(min(theta), max(theta), length.out = breaks)
  test_info = numeric(breaks)

  if (use_ltm == TRUE){
    n_item = ncol(object$X)#number of items (ltm of grm object)
  }
  if (use_ltm == FALSE){
    n_item = ncol(object@Data$data)#number of items (mirt object)
  }

  for (j in 1:n_item) {
    item_info = calculate_iic(object = object, theta = theta, breaks = breaks, item = j, grm = grm, use_ltm = use_ltm)
    test_info = test_info + item_info
  }

  return(test_info)
}

#' @title visualize Item Information Curves.
#' @description \code{gg_tic} Calculate the item information volume from the item parameter obtained from GRM.
#'
#' @param object mirt or grm object. If you use the grm object, you must set ltm = TRUE.
#' @param theta vector. The length must be 2. Specify the range of θ in the IRCCC to be visualized using c().
#' @param breaks numeric. Specifies the number of divisions of theta. For example, if you specify 1000, the range specified by theta will be divided into 1000 parts. The default is 100.
#' @param se logical. The default is TRUE. If you are not adding a SE to the test information curve, set it to FALSE.
#' @param grm logical. The default is TRUE. If you are using a binary IRT　(2PLM) object, set it to TRUE.
#' @param use_ltm logical. The default is FALSE. If you are using an object (either an ltm object or a grm object) from the ltm package, set this to TRUE.
#' @param output.data Logical. The default is FALSE. If TRUE, instead of visualizing IRCCC, the data that forms the basis of IRCCC is output in long format.
#' @return Visualize test information Curves using ggplot2 package.
#' @export
#' @examples
#' #library(mirt)
#' #mirt_object <- mirt(data, itemtype = "graded")
#'
#' #gg_iic(mirt_object, item = 1)
#' #gg <- gg_iic(mirt_object, item = 1)
#' #gg + theme_bw()
#'
#' #library(ltm)
#' #ltm_object <- ltm(data ~ z1, IRT.param = TRUE)
#'
#' #gg_iic(ltm_object, item = 1, theta = c(-5, 5), breaks = 1000, grm = FALSE, use_ltm = TRUE)
gg_tic <- function(object, theta = c(-3, 3), breaks = 100, se = TRUE, grm = TRUE, use_ltm = FALSE, output.data = FALSE) {
  test_info = calculate_test_info(object = object, theta = theta, breaks = breaks, grm = grm, use_ltm = use_ltm)
  # check arguments part below
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

  # check monochrome value
  if (is.logical(se) != TRUE) {
    stop("`se` must be TRUE or FALSE.")
  }

  # check output.data value
  if (is.logical(output.data) != TRUE) {
    stop("`output.data` must be TRUE or FALSE.")
  }

  # check grm value
  if (is.logical(grm) != TRUE) {
    stop("`grm` must be TRUE or FALSE.")
  }

  # check arguments part above

  thetas = seq(min(theta), max(theta), length.out = breaks)
  if (se == TRUE) {
    plot_data = data.frame(
      Theta = thetas,
      TestInformation = test_info,
      SE = 1/sqrt(test_info)
    )
  }

  if (se == FALSE) {
    plot_data = data.frame(
      Theta = thetas,
      TestInformation = test_info
    )
  }


  if(output.data == TRUE) {
    return(plot_data)
  }

  if(output.data == FALSE) {
    if(se == TRUE) {
      gg = ggplot(data = plot_data) +
        geom_line(aes(x = Theta, y = TestInformation, linetype = "Test Information"), size = 1) +
        geom_line(aes(x = Theta, y = SE, linetype = "SE"), size = 1)
      gg = gg + labs(title = "Test Information Curves",
                     x = "Theta", y = "Test Information") +
        scale_linetype_manual(
          name = "",
          values = c(
            "Test Information" = "solid",
            "SE" = "dotted")
        ) +
        theme_minimal(base_family = "Times", base_size = 12) +    #Font Size Conforming to APA Standards
        theme(
          axis.text = element_text(size = 10, color = "black"),   # Axis labels
          axis.title = element_text(size = 12),                   # Axis title
          panel.grid.major = element_line(size = 0.5, linetype = "solid", color = "gray80"),  # grid lines
          panel.grid.minor = element_blank(),                     # Hide minor grid lines
          panel.border = element_blank(),                         # Remove panel border lines
          axis.line = element_line(size = 0.8, color = "black"),  # Emphasize the axis
          legend.text = element_text(size = 12)                   # font size in legend
        )}
    if(se == FALSE) {
      gg = ggplot(data = plot_data) +
        geom_line(aes(x = Theta, y = TestInformation), size = 1)
      gg = gg + labs(title = "Test Information Curves",
                     x = "Theta", y = "Test Information") +
        theme_minimal(base_family = "Times", base_size = 12) +    #Font Size Conforming to APA Standards
        theme(
          axis.text = element_text(size = 10, color = "black"),   # Axis labels
          axis.title = element_text(size = 12),                   # Axis title
          panel.grid.major = element_line(size = 0.5, linetype = "solid", color = "gray80"),  # grid lines
          panel.grid.minor = element_blank(),                     # Hide minor grid lines
          panel.border = element_blank(),                         # Remove panel border lines
          axis.line = element_line(size = 0.8, color = "black"),  # Emphasize the axis
          legend.text = element_text(size = 12)                   # font size in legend
        )}
    return(gg)
  }
}
