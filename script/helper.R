
#' Keep a current selectInput value if still valid, else fall back
#'
#' @description
#' Used when a selectInput's `choices` are being replaced (e.g. switching
#' between Raw and Composite output-indicator lists): keeps the currently
#' selected value if it's still present in the new choices, otherwise uses
#' `fallback`. Passing this explicitly to `updateSelectInput(selected = ...)`
#' avoids relying on Shiny's implicit "leave selection unchanged" behavior,
#' which does not reliably survive a full choices replacement.
#'
#' @param current Current value of the input (e.g. `input$Output_indicator`).
#' @param choices New choices vector the input is being updated to.
#' @param fallback Value to use if `current` is not among `choices`.
#' @export
keep_or_default <- function(current, choices, fallback) {
  if (!is.null(current) && length(current) == 1 && current %in% choices) {
    current
  } else {
    fallback
  }
}

#' Download modal with CSV/XLSX buttons (Shiny)
#'
#' @description
#' **Edict:** Show a simple modal dialog that lets users download a table
#' as **CSV** or **XLSX** with one click. Designed for dashboards where a
#' compact, discoverable download UI is preferred over persistent buttons.
#'
#' @param divid   Character ID applied to a `<div>` wrapper inside the modal
#'   (useful for CSS hooks or finding the content with JS). Optional but kept
#'   for layout/control parity with your original function.
#' @param csvid   Output ID for the CSV download handler (used by `downloadHandler`).
#' @param xlsxid  Output ID for the XLSX download handler (used by `downloadHandler`).
#'
#' @return A `shiny.tag.list` (modal dialog) suitable for `shiny::showModal()`.
#'
#' @details
#' - **Dependencies:** [`shiny`](https://shiny.posit.co) and
#'   [`shinyWidgets::downloadBttn()`](https://dreamrs.github.io/shinyWidgets/).
#' - Pair with server-side `downloadHandler(outputId = csvid, ...)` and
#'   `downloadHandler(outputId = xlsxid, ...)`.
#' - Call with `showModal(DownloadModal("dl-box", "download_csv", "download_xlsx"))`.
#'
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#'   actionButton("open", "Open download modal")
#' )
#' server <- function(input, output, session) {
#'   observeEvent(input$open, {
#'     showModal(DownloadModal("dl-box", "download_csv", "download_xlsx"))
#'   })
#'
#'   output$download_csv <- downloadHandler(
#'     filename = function() "data.csv",
#'     content  = function(file) write.csv(mtcars, file, row.names = FALSE)
#'   )
#'
#'   output$download_xlsx <- downloadHandler(
#'     filename = function() "data.xlsx",
#'     content  = function(file) {
#'       # e.g., openxlsx::write.xlsx(mtcars, file)
#'     }
#'   )
#' }
#' shinyApp(ui, server)
#' }
#'
#' @importFrom shiny modalDialog br div
#' @importFrom shinyWidgets downloadBttn
#' @export
DownloadModal <- function(divid, csvid, xlsxid) {
  
  # Modal content: a div wrapper (id = divid) containing two download buttons.
  # Using shinyWidgets::downloadBttn for a cleaner, modern look.
  shiny::modalDialog(
    shiny::div(
      id = divid,
      
      shinyWidgets::downloadBttn(
        outputId = csvid,
        label    = "Download CSV",
        style    = "bordered",
        color    = "primary"
      ),
      
      shiny::br(), shiny::br(),
      
      shinyWidgets::downloadBttn(
        outputId = xlsxid,
        label    = "Download XLSX",
        style    = "bordered",
        color    = "primary"
      )
    ),
    easyClose = TRUE,
    title     = "Download Table"
  )
}

#' Generate coordinates of a circle (useful for PCA correlation circle)
#'
#' @description
#' **Edict:** This function creates the coordinates of a circle, typically
#' used to draw the unit correlation circle in PCA (Principal Component Analysis)
#' biplots or any visualization requiring a circular reference boundary.
#'
#' The function returns a data frame containing the `(x, y)` coordinates
#' of evenly spaced points along the circumference of a circle, defined by
#' its `center` and `diameter`.
#'
#' @param center Numeric vector of length 2 specifying the x and y coordinates
#'   of the circle's center (default: `c(0, 0)`).
#' @param diameter Numeric value specifying the circle's diameter (default: `1`).
#' @param npoints Integer specifying the number of points used to approximate
#'   the circle (default: `100`). Higher values produce a smoother circle.
#'
#' @return
#' A `data.frame` with two columns:
#' \itemize{
#'   \item `x` – x-coordinates of the circle,
#'   \item `y` – y-coordinates of the circle.
#' }
#'
#' @details
#' The circle is generated parametrically using:
#' \deqn{
#'   x = x_c + r \cos(\theta), \quad
#'   y = y_c + r \sin(\theta)
#' }
#' where \(r = \frac{diameter}{2}\) and \(\theta\) spans from 0 to \(2\pi\).
#'
#' @examples
#' # Create coordinates for a PCA correlation circle
#' circle_data <- circleFun(center = c(0, 0), diameter = 2, npoints = 200)
#'
#' # Plot using ggplot2
#' library(ggplot2)
#' ggplot(circle_data, aes(x, y)) +
#'   geom_path(color = "blue") +
#'   coord_equal() +
#'   theme_minimal()
#'
#' @export
circleFun <- function(center = c(0, 0), diameter = 1, npoints = 100) {
  # -------------------------------------------------------------------------
  # Edict: Generate coordinates of a circle centered at 'center'
  # with given 'diameter', returning npoints evenly spaced along the perimeter.
  # Commonly used for PCA biplots (correlation circle) or general visualization.
  # -------------------------------------------------------------------------
  
  # Compute radius from diameter
  r <- diameter / 2
  
  # Generate angles (0 to 2π) evenly spaced
  tt <- seq(0, 2 * pi, length.out = npoints)
  
  # Compute x and y coordinates of the circle
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  
  # Return as a data.frame (for easy plotting in ggplot)
  return(data.frame(x = xx, y = yy))
}





#' Normalize a numeric vector to the range [-2.5, 2.5]
#'
#' @description
#' **Edict:** Linearly rescales a numeric vector so that its minimum maps to **-2.5**
#' and its maximum maps to **+2.5**. This is useful for putting heterogeneous
#' indicators on a common, centered scale for visualization or aggregation.
#'
#' The transformation is:
#' \deqn{
#'   \text{res} = 5 \cdot \frac{x - \min(x)}{\max(x) - \min(x)} - 2.5
#' }
#' (computed with `na.rm = TRUE` for the min/max).
#'
#' @param x Numeric vector (may contain `NA`s).
#'
#' @return A numeric vector of the same length as `x`, with values linearly
#' rescaled to \([-2.5, 2.5]\). `NA`s in `x` are preserved.
#'
#' @details
#' - If `max(x, na.rm = TRUE) == min(x, na.rm = TRUE)` (i.e., `x` is constant
#'   up to `NA`s), the denominator is zero. The original implementation would
#'   yield `NaN/Inf`. Consider handling this upstream or adapting the function
#'   to return zeros in that case (see note below).
#'
#' @examples
#' translateFunc(c(10, 20, 30))        # -> [-2.5, 0, 2.5]
#' translateFunc(c(NA, 1, 4, 7, NA))   # NAs preserved, others scaled
#'
#' @export
translateFunc <- function(x) { # normalized function
  # Compute range endpoints ignoring NAs
  Min <- min(x, na.rm = TRUE)
  Max <- max(x, na.rm = TRUE)
  
  # NOTE: If Max == Min, denominator is zero (constant vector case).
  # Current behavior (kept to match your original): this will produce Inf/NaN.
  # If you prefer safe behavior, replace the next line with a guard, e.g.:
  # if (is.finite(Min) && is.finite(Max) && Max == Min) return(rep(0, length(x)))
  
  # Linear rescale so Min -> -2.5 and Max -> +2.5
  # Equivalent compact form: 5 * (x - Min) / (Max - Min) - 2.5
  if (is.finite(Min) && is.finite(Max) && Max == Min) {
    return(ifelse(is.na(x), NA_real_, 0))
  } else{
    return((5 / (Max - Min)) * x - 2.5 - (5 / (Max - Min)) * Min)
  }
  
  
  return(res)
}


#' Row of selected percentiles for a numeric vector
#'
#' @description
#' **Edict:** Compute a tidy, single-row summary of percentiles (P5, P10, …, P95)
#' for a numeric vector. Useful for quick distribution snapshots in reports/tables.
#'
#' @param var Numeric vector (may include `NA`s).
#'
#' @return
#' A one-row `data.frame` whose columns are the percentiles:
#' `P5, P10, P15, ..., P95`.
#'
#' @details
#' Uses `quantile()` with `na.rm = TRUE` and its default type (type = 7).
#' Percentile probabilities are `seq(0.05, 0.95, by = 0.05)`.
#'
#' @examples
#' set.seed(1)
#' x <- c(rnorm(100), NA)
#' Perfunc(x)
#'
#' @export
Perfunc <- function(var) { # Percentile function
  # Probabilities for 5th to 95th percentiles by 5%
  probs <- seq(0.05, 0.95, by = 0.05)
  
  # Compute the quantiles; na.rm = TRUE ignores NAs
  qs <- quantile(var, prob = probs, na.rm = TRUE)
  
  # Put results in a single-row data.frame with desired column names
  res <- data.frame(
    matrix(qs, nrow = 1, ncol = length(probs), byrow = TRUE)
  )
  colnames(res) <- paste0("P", seq(5, 95, by = 5))
  
  return(res)
}




#' Quantile summary as a tidy tibble
#'
#' @description
#' **Edict:** Compute selected quantiles of a numeric vector and return a tidy
#' tibble with the numeric value, a human-readable label (e.g., "25th percentile"),
#' and the original probability (`probs`).
#'
#' @param x Numeric vector (may contain `NA`s).
#' @param probs Numeric vector of probabilities in \[0, 1\] at which to compute
#'   the quantiles (e.g., `c(0.25, 0.5, 0.75)`).
#'
#' @return
#' A tibble with columns:
#' - `Percent_value`: the quantile values,
#' - `Percentile`: a label like `"25th percentile"`,
#' - `Percentile_T`: the original probability in \[0,1\].
#'
#' @details
#' - Uses `quantile(x, probs, na.rm = TRUE)` with default interpolation (`type = 7`).
#' - `probs` outside \[0,1\] are not allowed and will trigger an error.
#' - If all elements of `x` are `NA`, returned values will be `NA`.
#'
#' @examples
#' quantile_func(1:10, c(0.25, 0.5, 0.75))
#' quantile_func(c(NA, 3, 5, 9, NA), seq(0.1, 0.9, by = 0.2))
#'
#' @importFrom tibble tibble
#' @export
quantile_func <- function(x, probs) {
  # ---- Basic validation -------------------------------------------------------
  if (!is.numeric(x)) stop("`x` must be numeric.")
  if (!is.numeric(probs)) stop("`probs` must be a numeric vector.")
  if (any(is.na(probs))) stop("`probs` cannot contain NA.")
  if (any(probs < 0 | probs > 1)) stop("All `probs` must lie in [0, 1].")
  
  # Compute quantiles; names attribute may be attached by base::quantile
  qv <- quantile(x, probs = probs, na.rm = TRUE)
  
  # Build readable percentile labels (e.g., 0.25 -> "25th percentile")
  pct_labels <- paste0(round(probs * 100, 0), "th percentile")
  
  # Return as a tidy tibble. Ensure numeric vector (drop names) with as.numeric()
  tibble::tibble(
    Percent_value = as.numeric(qv),
    Percentile    = pct_labels,
    Percentile_T  = probs
  )
}




