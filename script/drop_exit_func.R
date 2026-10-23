
#' Forward-fill missing indicators conditional on `below_start_p`
#'
#' @description
#' Completes a country-year row block by **forward-filling** the indicator
#' variables (`below_start_p`, `above_exit_p`, `below_exit_p`) *only when*
#' `below_start_p` is `NA` at time *t* **and** non-`NA` at time *t-1*. In that
#' case, the function copies all three indicators from *t-1* to *t*.  
#' This preserves your intended behavior: the existence of a prior, known
#' `below_start_p` acts as the gate to borrow last period’s full indicator state.
#'
#' @param year Integer/numeric vector of years (assumed sorted ascending and
#'   contiguous by the caller).
#' @param below_start_p,above_exit_p,below_exit_p Numeric/logical 0/1 vectors of
#'   equal length with possible `NA`s.
#'
#' @return A data frame with columns
#'   `year`, `below_start_p`, `above_exit_p`, `below_exit_p`, where some `NA`s
#'   may have been forward-filled according to the rule above.
#'
#' @details
#' - Only rows with `is.na(below_start_p[t]) && !is.na(below_start_p[t-1])`
#'   are filled. All three indicators at *t* are set to their values at *t-1*.
#' - The first row is never modified (no t-1).
#' - No general forward-fill is applied; the rule is **conditional** on
#'   `below_start_p` being the trigger.
#'
#' @examples
#' # fill_missing(year, below_start_p, above_exit_p, below_exit_p)
#'
#' @export
fill_missing <- function(year,
                         below_start_p,
                         above_exit_p,
                         below_exit_p) {
  # Iterate from the second observation onward
  for (i in 2:length(year)) {
    # If the current below_start_p is missing but the previous is known,
    # copy the entire indicator state from t-1 to t.
    if (is.na(below_start_p[i]) & !is.na(below_start_p[i - 1])) {
      below_start_p[i] <- below_start_p[i - 1]
      below_exit_p[i]  <- below_exit_p[i - 1]
      above_exit_p[i]  <- above_exit_p[i - 1]
    }
  }
  
  # Return as a data.frame (kept to match your downstream usage)
  res <- data.frame(
    year          = year,
    below_start_p = below_start_p,
    above_exit_p  = above_exit_p,
    below_exit_p  = below_exit_p
  )
  return(res)
}




#' Detect "exit" and "fragility entry" episodes by country over time
#'
#' @description
#' Scans each country's year-by-year series once, in chronological order, and
#' records **every** qualifying regime transition (not just the first):
#' - **Exit**: a confirmed fragile regime is followed, at some later point,
#'   by `TIME_THRESHOLD` consecutive years of **above_exit_p == 1** (no
#'   distinction is made between an immediate vs. a delayed transition).
#' - **Fragility Entry**: a confirmed non-fragile regime is followed, at some
#'   later point, by `TIME_THRESHOLD` consecutive years of
#'   **below_start_p == 1**. Named "Entry" rather than "Relapse" because the
#'   event only asserts a transition from confirmed non-fragile to confirmed
#'   fragile -- it does not assert (and the data often cannot establish) that
#'   the country was fragile at some earlier point too, which "relapse" would
#'   imply.
#'
#' A country's **starting regime** is established by the *first* run of
#' `TIME_THRESHOLD` consecutive years of *either* `below_start_p == 1` or
#' `above_exit_p == 1`, whichever occurs first chronologically -- establishing
#' the baseline is not itself an event. (Earlier versions of this function
#' required the starting run to specifically be `below_start_p`, which meant a
#' country whose first-ever confirmed `TIME_THRESHOLD`-year run was an
#' `above_exit_p` spell could never have that spell -- or the relapse that
#' later followed it -- correctly logged as a dated pair.)
#'
#' From the starting regime onward, the scan alternates: each time it finds
#' the *next* `TIME_THRESHOLD`-year run of the opposite type, it logs that
#' transition (dated at the first year of the run) and continues searching
#' for the *next* transition after that run -- so a country that exits,
#' relapses, and exits again gets all of those episodes, not just the first
#' of each type.
#'
#' Missing years in each country's panel are completed to a full sequence and
#' then filled using `fill_missing()`, so the detection logic always runs on
#' a contiguous, year-by-year series.
#'
#' @param df A data frame/tibble with columns:
#'   - `Country` (factor/character),
#'   - `year` (integer/numeric),
#'   - `below_start_p`, `above_exit_p`, `below_exit_p` (binary 0/1 indicators).
#'     Only `below_start_p` and `above_exit_p` are used in the episode definitions here.
#' @param TIME_THRESHOLD Integer window length (e.g., 2, 3, …) used for run detection
#'   (consecutive years required to flag an episode).
#'
#' @return A tibble with columns:
#'   - `Year`: the first year of the detected window described in `Type`,
#'   - `Type`: one of `"Exit"` or `"Fragility Entry"`,
#'   - `Country`: country identifier.
#'
#' @examples
#' # drop_exit_func(my_panel_df, TIME_THRESHOLD = 3)
#'
#' @export
drop_exit_func <- function(df, TIME_THRESHOLD) {
  # ---- Basic validation -------------------------------------------------------
  stopifnot(is.data.frame(df))
  if (!all(c("Country","year","below_start_p","above_exit_p","below_exit_p") %in% names(df))) {
    stop("`df` must include columns: Country, year, below_start_p, above_exit_p, below_exit_p.")
  }
  if (!is.numeric(TIME_THRESHOLD) || length(TIME_THRESHOLD) != 1 || TIME_THRESHOLD < 1) {
    stop("`TIME_THRESHOLD` must be a single positive integer.")
  }

  country_list <- df |>
    dplyr::ungroup() |>
    dplyr::distinct(Country) |>
    dplyr::pull(Country)

  all_events <- NULL

  for (country_name in country_list) {

    df_country <- df |>
      dplyr::ungroup() |>
      dplyr::filter(Country == country_name) |>
      dplyr::select(year, below_start_p, above_exit_p, below_exit_p)

    # Complete yearly sequence and fill missing indicators
    df_country <- df_country |>
      dplyr::reframe(year = seq(from = min(year), to = max(year), by = 1)) |>
      dplyr::left_join(df_country, by = "year") |>
      dplyr::arrange(year) |>
      dplyr::reframe(fill_missing(year, below_start_p, above_exit_p, below_exit_p))

    year          <- df_country |> dplyr::pull(year)
    below_start_p <- df_country |> dplyr::pull(below_start_p)
    above_exit_p  <- df_country |> dplyr::pull(above_exit_p)
    N <- length(year)

    if (N < TIME_THRESHOLD) next

    is_run <- function(x, start) {
      end <- start + TIME_THRESHOLD - 1
      end <= N && !anyNA(x[start:end]) && all(x[start:end] == 1)
    }

    # ---- Establish the starting regime: first T-year run of either type ----
    state <- NA_character_   # "fragile" or "nonfragile"
    block_end <- NA_integer_
    for (i in seq_len(N - TIME_THRESHOLD + 1)) {
      if (is_run(below_start_p, i)) {
        state <- "fragile"; block_end <- i + TIME_THRESHOLD - 1; break
      }
      if (is_run(above_exit_p, i)) {
        state <- "nonfragile"; block_end <- i + TIME_THRESHOLD - 1; break
      }
    }
    if (is.na(state)) next  # no confirmed regime ever observed for this country

    # ---- Walk forward, logging every subsequent transition -----------------
    repeat {
      target <- if (state == "fragile") above_exit_p else below_start_p
      search_from <- block_end + 1
      if (search_from > N - TIME_THRESHOLD + 1) break

      next_start <- NA_integer_
      for (j in search_from:(N - TIME_THRESHOLD + 1)) {
        if (is_run(target, j)) { next_start <- j; break }
      }
      if (is.na(next_start)) break  # no further qualifying transition

      if (state == "fragile") {
        event_type <- "Exit"
        new_state <- "nonfragile"
      } else {
        event_type <- "Fragility Entry"
        new_state <- "fragile"
      }

      all_events <- dplyr::bind_rows(
        all_events,
        data.frame(Year = year[next_start], Type = event_type, Country = country_name)
      )

      state <- new_state
      block_end <- next_start + TIME_THRESHOLD - 1
    }
  }

  if (is.null(all_events)) {
    return(data.frame(Year = numeric(0), Type = character(0), Country = character(0)))
  }
  all_events
}
