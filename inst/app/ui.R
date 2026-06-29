ui <- page_navbar(
  title = "PropCIs — Confidence Intervals for Proportions",
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Inter")),
  fillable = FALSE,

  # ── Tab 1: Single Proportion ────────────────────────────────────────────────
  nav_panel(
    "Single Proportion",
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        h5("Inputs"),
        numericInput("sp_x",    "Successes (x)",     value = 5,    min = 0, step = 1),
        numericInput("sp_n",    "Sample size (n)",   value = 20,   min = 1, step = 1),
        sliderInput( "sp_conf", "Confidence level",  min = 0.80, max = 0.99,
                     value = 0.95, step = 0.01),
        hr(),
        selectInput("sp_method", "Method",
                    choices = c(
                      "Wilson (Score)"           = "wilson",
                      "Clopper-Pearson (Exact)"  = "exact",
                      "mid-P"                    = "midp",
                      "Blaker"                   = "blaker",
                      "Agresti-Coull (Add-4)"    = "add4",
                      "Agresti-Coull (Add-z²/2)" = "addz2"
                    )),
        helpText("Commit to one method before inspecting results to maintain the nominal Type I error rate.")
      ),
      card(
        card_header("Result"),
        DTOutput("sp_table")
      ),
      card(
        card_header("Confidence Interval"),
        plotOutput("sp_plot", height = "200px")
      )
    )
  ),

  # ── Tab 2: Two Independent Proportions ─────────────────────────────────────
  nav_panel(
    "Two Independent Proportions",
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        h5("Group 1"),
        numericInput("tp_x1", "Successes (x1)",   value = 10, min = 0, step = 1),
        numericInput("tp_n1", "Sample size (n1)",  value = 50, min = 1, step = 1),
        h5("Group 2"),
        numericInput("tp_x2", "Successes (x2)",   value = 15, min = 0, step = 1),
        numericInput("tp_n2", "Sample size (n2)",  value = 50, min = 1, step = 1),
        sliderInput( "tp_conf", "Confidence level", min = 0.80, max = 0.99,
                     value = 0.95, step = 0.01),
        hr(),
        selectInput("tp_choice", "Measure & Method",
                    choices = c(
                      "Difference — Score (Mee)" = "diff_score",
                      "Difference — Wald"        = "diff_wald",
                      "Difference — Wald AC"     = "diff_waldac",
                      "Relative Risk — Score"    = "rr_score",
                      "Odds Ratio — Score"       = "or_score"
                    )),
        helpText("Commit to one method before inspecting results to maintain the nominal Type I error rate.")
      ),
      card(
        card_header("Result"),
        DTOutput("tp_table")
      ),
      card(
        card_header("Confidence Interval"),
        plotOutput("tp_plot", height = "200px")
      )
    )
  ),

  # ── Tab 3: Matched Pairs ────────────────────────────────────────────────────
  nav_panel(
    "Matched Pairs",
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        h5("Discordant cell counts"),
        numericInput("mp_b",    "b  (−/+) pairs",   value = 5,  min = 0, step = 1),
        numericInput("mp_c",    "c  (+/−) pairs",   value = 10, min = 0, step = 1),
        numericInput("mp_n",    "Total pairs (n)",  value = 30, min = 1, step = 1),
        sliderInput( "mp_conf", "Confidence level", min = 0.80, max = 0.99,
                     value = 0.95, step = 0.01),
        hr(),
        selectInput("mp_choice", "Measure & Method",
                    choices = c(
                      "Difference — Adjusted Wald" = "diff_adjwald",
                      "Difference — Wald"          = "diff_wald",
                      "Difference — Tango Score"   = "diff_score",
                      "Odds Ratio — Score"         = "or_score"
                    )),
        helpText(
          tags$small(
            "Row = Treatment 1, Column = Treatment 2:",
            tags$table(
              style = "border-collapse:collapse; margin:4px 0;",
              tags$tr(
                tags$th(""),
                tags$th("Col +", style = "padding:2px 6px; border:1px solid #999; font-weight:normal; font-style:italic;"),
                tags$th("Col −", style = "padding:2px 6px; border:1px solid #999; font-weight:normal; font-style:italic;")
              ),
              tags$tr(
                tags$th("Row +", style = "padding:2px 6px; border:1px solid #999; font-weight:normal; font-style:italic;"),
                tags$td("a",        style = "padding:2px 6px; border:1px solid #999; text-align:center;"),
                tags$td(tags$b("c"), style = "padding:2px 6px; border:1px solid #999; text-align:center;")
              ),
              tags$tr(
                tags$th("Row −", style = "padding:2px 6px; border:1px solid #999; font-weight:normal; font-style:italic;"),
                tags$td(tags$b("b"), style = "padding:2px 6px; border:1px solid #999; text-align:center;"),
                tags$td("d",        style = "padding:2px 6px; border:1px solid #999; text-align:center;")
              )
            ),
            tags$b("b"), " and ", tags$b("c"), " are the discordant pairs entered above;",
            " a and d (concordant) count only toward n.",
            tags$br(),
            "Positive difference and OR > 1 favour the row treatment.",
            tags$br(),
            "Commit to one method before inspecting results."
          )
        )
      ),
      card(
        card_header("Result"),
        DTOutput("mp_table")
      ),
      card(
        card_header("Confidence Interval"),
        plotOutput("mp_plot", height = "200px")
      )
    )
  ),

  # ── Tab 4: Bayesian ─────────────────────────────────────────────────────────
  nav_panel(
    "Bayesian (Two Proportions)",
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        h5("Group 1"),
        numericInput("ba_x1", "Successes (x1)",   value = 10, min = 0, step = 1),
        numericInput("ba_n1", "Sample size (n1)",  value = 50, min = 1, step = 1),
        h5("Group 2"),
        numericInput("ba_x2", "Successes (x2)",   value = 15, min = 0, step = 1),
        numericInput("ba_n2", "Sample size (n2)",  value = 50, min = 1, step = 1),
        sliderInput( "ba_conf", "Confidence level", min = 0.80, max = 0.99,
                     value = 0.95, step = 0.01),
        hr(),
        selectInput("ba_measure", "Measure",
                    choices = c(
                      "Difference (p1−p2)"    = "diff",
                      "Relative Risk (p1/p2)" = "rr",
                      "Odds Ratio"             = "or"
                    )),
        h6("Beta prior  —  Group 1: Beta(a, b)"),
        fluidRow(
          column(6, numericInput("ba_a", "a (α2)", value = 0.5, min = 0.01, step = 0.5)),
          column(6, numericInput("ba_b", "b (β1)", value = 0.5, min = 0.01, step = 0.5))
        ),
        h6("Beta prior  —  Group 2: Beta(c, d)"),
        fluidRow(
          column(6, numericInput("ba_c", "c (α2)", value = 0.5, min = 0.01, step = 0.5)),
          column(6, numericInput("ba_d", "d (β2)", value = 0.5, min = 0.01, step = 0.5))
        ),
        numericInput("ba_nsim", "Simulations (nsim)", value = 1e5,
                     min = 1000, max = 1e7, step = 1e4),
        actionButton("ba_run", "Compute", class = "btn-primary w-100 mt-2"),
        hr(),
        helpText("Select measure before clicking Compute. Increase nsim for higher accuracy.")
      ),
      card(
        card_header("Result"),
        DTOutput("ba_table")
      ),
      card(
        card_header("Confidence Interval"),
        plotOutput("ba_plot", height = "200px")
      )
    )
  ),

  # ── Tab 5: About ────────────────────────────────────────────────────────────
  nav_panel(
    "About",
    card(
      card_body(
        h4("PropCIs Shiny App"),
        p("This app exposes all user-facing confidence interval methods from the",
          tags$a("PropCIs", href = "https://cran.r-project.org/package=PropCIs",
                 target = "_blank"), "R package (Scherer, 2018)."),
        p("Only one method is computed at a time. Computing all methods simultaneously and",
          "selecting the most favourable result inflates the Type I error; committing to a",
          "method before inspecting the data maintains the nominal coverage."),
        h5("Single Proportion"),
        tags$ul(
          tags$li(tags$b("Wilson (Score)"), " — scoreci()"),
          tags$li(tags$b("Clopper-Pearson"), " — exactci()"),
          tags$li(tags$b("mid-P"), " — midPci()"),
          tags$li(tags$b("Blaker"), " — blakerci()"),
          tags$li(tags$b("Agresti-Coull (Add-4)"), " — add4ci()"),
          tags$li(tags$b("Agresti-Coull (Add-z²/2)"), " — addz2ci()")
        ),
        h5("Two Independent Proportions"),
        tags$ul(
          tags$li(tags$b("Difference: Score"), " — diffscoreci()"),
          tags$li(tags$b("Difference: Wald / AC"), " — wald2ci()"),
          tags$li(tags$b("Relative Risk: Score"), " — riskscoreci()"),
          tags$li(tags$b("Odds Ratio: Score"), " — orscoreci()")
        ),
        h5("Matched Pairs"),
        tags$ul(
          tags$li(tags$b("Difference: Adjusted Wald"), " — diffpropci.mp()"),
          tags$li(tags$b("Difference: Wald"), " — diffpropci.Wald.mp()"),
          tags$li(tags$b("Difference: Tango Score"), " — scoreci.mp()"),
          tags$li(tags$b("Odds Ratio: Score"), " — oddsratioci.mp()")
        ),
        h5("Bayesian (Monte Carlo, two proportions)"),
        tags$ul(
          tags$li(tags$b("Difference"), " — diffci.bayes()"),
          tags$li(tags$b("Relative Risk"), " — rrci.bayes()"),
          tags$li(tags$b("Odds Ratio"), " — orci.bayes()")
        ),
        hr(),
        p(tags$em("Scherer, R. (2018). PropCIs: Various Confidence Interval Methods for Proportions. R package version 0.3-0."))
      )
    )
  )
)
