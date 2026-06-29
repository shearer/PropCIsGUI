server <- function(input, output, session) {

  # ── Single Proportion ───────────────────────────────────────────────────────

  sp_data <- reactive({
    x  <- input$sp_x
    n  <- input$sp_n
    cl <- input$sp_conf
    validate(
      need(is.finite(x) && x >= 0, "x must be a non-negative number."),
      need(is.finite(n) && n >= 1, "n must be at least 1."),
      need(x <= n,                  "x cannot exceed n.")
    )
    est <- x / n
    res <- switch(input$sp_method,
      wilson = tryCatch(scoreci(x, n, cl),  error = function(e) NULL),
      exact  = tryCatch(exactci(x, n, cl),  error = function(e) NULL),
      midp   = tryCatch(midPci(x, n, cl),   error = function(e) NULL),
      blaker = tryCatch(blakerci(x, n, cl), error = function(e) NULL),
      add4   = tryCatch(add4ci(x, n, cl),   error = function(e) NULL),
      addz2  = tryCatch(addz2ci(x, n, cl),  error = function(e) NULL)
    )
    req(!is.null(res))
    label <- switch(input$sp_method,
      wilson = "Wilson (Score)",
      exact  = "Clopper-Pearson (Exact)",
      midp   = "mid-P",
      blaker = "Blaker",
      add4   = "Agresti-Coull (Add-4)",
      addz2  = "Agresti-Coull (Add-z²/2)"
    )
    ci_row("Single proportion", label, est, res$conf.int[1], res$conf.int[2])
  })

  output$sp_table <- renderDT({
    df <- sp_data()
    datatable(df[, c("Method", "Estimate", "Lower", "Upper", "Width")],
              rownames = FALSE,
              options  = list(dom = "t", pageLength = 10, ordering = FALSE))
  })

  output$sp_plot <- renderPlot({
    df <- sp_data()
    req(nrow(df) > 0)
    ci_plot(df,
            title     = paste0(round(input$sp_conf * 100),
                               "% CI — Single Proportion  (x=",
                               input$sp_x, ", n=", input$sp_n, ")"),
            null_vals = NULL) +
      scale_x_continuous(limits = c(0, 1),
                         labels = scales::percent_format(accuracy = 1))
  })

  # ── Two Independent Proportions ─────────────────────────────────────────────

  tp_data <- reactive({
    x1 <- input$tp_x1;  n1 <- input$tp_n1
    x2 <- input$tp_x2;  n2 <- input$tp_n2
    cl <- input$tp_conf
    validate(
      need(is.finite(x1) && x1 >= 0 && x1 <= n1, "Group 1: x₁ must be in [0, n₁]."),
      need(is.finite(x2) && x2 >= 0 && x2 <= n2, "Group 2: x₂ must be in [0, n₂]."),
      need(n1 >= 1 && n2 >= 1,                     "Both sample sizes must be ≥ 1.")
    )
    p1 <- x1 / n1;  p2 <- x2 / n2

    switch(input$tp_choice,
      diff_score = {
        res <- tryCatch(diffscoreci(x1, n1, x2, n2, cl), error = function(e) NULL)
        req(!is.null(res))
        ci_row("Difference (p1-p2)", "Score (Mee)", p1 - p2,
               res$conf.int[1], res$conf.int[2])
      },
      diff_wald = {
        res <- tryCatch(wald2ci(x1, n1, x2, n2, cl, adjust = "Wald"), error = function(e) NULL)
        req(!is.null(res))
        ci_row("Difference (p1-p2)", "Wald", p1 - p2,
               res$conf.int[1], res$conf.int[2])
      },
      diff_waldac = {
        res <- tryCatch(wald2ci(x1, n1, x2, n2, cl, adjust = "AC"), error = function(e) NULL)
        req(!is.null(res))
        ci_row("Difference (p1-p2)", "Wald AC", p1 - p2,
               res$conf.int[1], res$conf.int[2])
      },
      rr_score = {
        res <- tryCatch(riskscoreci(x1, n1, x2, n2, cl), error = function(e) NULL)
        req(!is.null(res))
        rr <- if (p2 > 0) p1 / p2 else NA_real_
        ci_row("Relative Risk (p1/p2)", "Score", rr,
               res$conf.int[1], res$conf.int[2])
      },
      or_score = {
        res <- tryCatch(orscoreci(x1, n1, x2, n2, cl), error = function(e) NULL)
        req(!is.null(res))
        denom <- x2 * (n1 - x1)
        or <- if (denom > 0) x1 * (n2 - x2) / denom else NA_real_
        ci_row("Odds Ratio", "Score", or,
               res$conf.int[1], res$conf.int[2])
      }
    )
  })

  output$tp_table <- renderDT({
    df <- tp_data()
    datatable(df[, c("Measure", "Method", "Estimate", "Lower", "Upper", "Width")],
              rownames = FALSE,
              options  = list(dom = "t", pageLength = 10, ordering = FALSE))
  })

  output$tp_plot <- renderPlot({
    df <- tp_data()
    req(nrow(df) > 0)
    null_map <- c(diff_score = 0, diff_wald = 0, diff_waldac = 0, rr_score = 1, or_score = 1)
    ci_plot(df,
            title     = paste0(round(input$tp_conf * 100),
                               "% CI — Two Independent Proportions  (x1=", input$tp_x1,
                               "/n1=", input$tp_n1, ", x2=", input$tp_x2,
                               "/n2=", input$tp_n2, ")"),
            null_vals = null_map[input$tp_choice])
  })

  # ── Matched Pairs ────────────────────────────────────────────────────────────

  mp_data <- reactive({
    b  <- input$mp_b
    cc <- input$mp_c
    n  <- input$mp_n
    cl <- input$mp_conf
    validate(
      need(is.finite(b)  && b  >= 0, "b must be a non-negative integer."),
      need(is.finite(cc) && cc >= 0, "c must be a non-negative integer."),
      need(is.finite(n)  && n  >= 1, "n must be at least 1."),
      need(b + cc <= n,               "b + c cannot exceed n.")
    )

    switch(input$mp_choice,
      diff_adjwald = {
        res <- tryCatch(diffpropci.mp(b, cc, n, cl), error = function(e) NULL)
        req(!is.null(res))
        est <- if (!is.null(res$estimate)) res$estimate else (cc - b) / n
        ci_row("Difference (p1-p2)", "Adjusted Wald", est,
               res$conf.int[1], res$conf.int[2])
      },
      diff_wald = {
        res <- tryCatch(diffpropci.Wald.mp(b, cc, n, cl), error = function(e) NULL)
        req(!is.null(res))
        est <- if (!is.null(res$estimate)) res$estimate else (cc - b) / n
        ci_row("Difference (p1-p2)", "Wald", est,
               res$conf.int[1], res$conf.int[2])
      },
      diff_score = {
        res <- tryCatch(scoreci.mp(b, cc, n, cl), error = function(e) NULL)
        req(!is.null(res))
        est <- if (!is.null(res$estimate)) res$estimate else (cc - b) / n
        ci_row("Difference (p1-p2)", "Tango Score", est,
               res$conf.int[1], res$conf.int[2])
      },
      or_score = {
        validate(need(b > 0, "b must be > 0 to compute an odds ratio."))
        res <- tryCatch(oddsratioci.mp(b, cc, cl), error = function(e) NULL)
        req(!is.null(res))
        ci_row("Odds Ratio (matched)", "Score", cc / b,
               res$conf.int[1], res$conf.int[2])
      }
    )
  })

  output$mp_table <- renderDT({
    df <- mp_data()
    datatable(df[, c("Measure", "Method", "Estimate", "Lower", "Upper", "Width")],
              rownames = FALSE,
              options  = list(dom = "t", pageLength = 10, ordering = FALSE))
  })

  output$mp_plot <- renderPlot({
    df <- mp_data()
    req(nrow(df) > 0)
    null_map <- c(diff_adjwald = 0, diff_wald = 0, diff_score = 0, or_score = 1)
    ci_plot(df,
            title     = paste0(round(input$mp_conf * 100),
                               "% CI — Matched Pairs  (b=", input$mp_b,
                               ", c=", input$mp_c, ", n=", input$mp_n, ")"),
            null_vals = null_map[input$mp_choice])
  })

  # ── Bayesian ─────────────────────────────────────────────────────────────────

  ba_data <- eventReactive(input$ba_run, {
    x1 <- input$ba_x1;  n1 <- input$ba_n1
    x2 <- input$ba_x2;  n2 <- input$ba_n2
    cl <- input$ba_conf
    a  <- input$ba_a;   b  <- input$ba_b
    cc <- input$ba_c;   d  <- input$ba_d
    ns <- as.integer(input$ba_nsim)
    validate(
      need(x1 >= 0 && x1 <= n1, "Group 1: x₁ must be in [0, n₁]."),
      need(x2 >= 0 && x2 <= n2, "Group 2: x₂ must be in [0, n₂]."),
      need(a > 0 && b > 0 && cc > 0 && d > 0, "All prior parameters must be > 0.")
    )
    p1 <- x1 / n1;  p2 <- x2 / n2

    switch(input$ba_measure,
      diff = {
        res <- tryCatch(diffci.bayes(x1, n1, x2, n2, a, b, cc, d, cl, ns),
                        error = function(e) NULL)
        req(!is.null(res))
        ci_row("Difference (p1-p2)", "Bayesian tail", p1 - p2, res[1], res[2])
      },
      rr = {
        res <- tryCatch(rrci.bayes(x1, n1, x2, n2, a, b, cc, d, cl, ns),
                        error = function(e) NULL)
        req(!is.null(res))
        rr <- if (p2 > 0) p1 / p2 else NA_real_
        ci_row("Relative Risk (p1/p2)", "Bayesian tail", rr, res[1], res[2])
      },
      or = {
        res <- tryCatch(orci.bayes(x1, n1, x2, n2, a, b, cc, d, cl, ns),
                        error = function(e) NULL)
        req(!is.null(res))
        denom <- x2 * (n1 - x1)
        or <- if (denom > 0) x1 * (n2 - x2) / denom else NA_real_
        ci_row("Odds Ratio", "Bayesian tail", or, res[1], res[2])
      }
    )
  })

  output$ba_table <- renderDT({
    df <- ba_data()
    datatable(df[, c("Measure", "Method", "Estimate", "Lower", "Upper", "Width")],
              rownames = FALSE,
              options  = list(dom = "t", pageLength = 10, ordering = FALSE))
  })

  output$ba_plot <- renderPlot({
    df <- ba_data()
    req(nrow(df) > 0)
    null_map <- c(diff = 0, rr = 1, or = 1)
    ci_plot(df,
            title     = paste0(round(input$ba_conf * 100),
                               "% Bayesian Credible Interval  (nsim=",
                               formatC(input$ba_nsim, format = "d", big.mark = ","), ")"),
            null_vals = null_map[input$ba_measure])
  })
}
