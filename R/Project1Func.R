#' @title Project 1 DDT Plot
#'
#' @param df df variable as the chosen data frame
#' @param species speices assigned to the condition statement
#' @param x x variable as a parameter in the data frame assigned to the x-axis variable
#' @param y y variable as a parameter in the data frame assigned to the y-axis variable
#' @param col col variable as an parameter to assign color to the scatter plot
#'
#'
#' @return Returns the data frame, the relative frequency of a chosen parameter, the subset data frame, and a scatter plot of the data with a quadratic model.
#' @export
#'
#' @import Intro2R
#'
#' @examples
#' \dontrun{myddt(df=ddt, species="CCATFISH")}
myddt <- function(df,species, x="LENGTH", y="WEIGHT", col="RIVER"){

  cond <- df$SPECIES == species
  df1 <- magrittr::`%>%`(df, dplyr::filter({{cond}}))
  g <- ggplot2::ggplot(df1, ggplot2::aes_string(x=x,y=y)) +
    ggplot2::geom_point(ggplot2::aes_string(color = col )) +
    ggplot2::geom_smooth(formula = y~x +I(x^2), method = "lm") +
    ggplot2::labs(title="Kendall Allsup")
  tab <- table(df$RIVER)/length(df$RIVER)

  print(g)
  print(tab)

  filename <- sprintf("LvsWfor%s.csv", species)
  utils::write.csv(df1, file=filename)

  list(DataFrame=df, SubsetDataFrame=df1)

}
