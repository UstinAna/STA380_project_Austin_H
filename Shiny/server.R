library(shiny)
library(ggplot2)
library(PermutationTests)
library(shinyjs)

server <- function(input, output, session) {

  run_id    <- reactiveVal(0)
  cancel_id <- reactiveVal(0)
  tabs_rendered <- reactiveVal(character(0))

  is_cancelled <- function() cancel_id() >= run_id() && run_id() > 0

  show_overlay <- function() {
    shinyjs::runjs('
      document.getElementById("loading_overlay").style.display = "flex";
      document.getElementById("run").disabled = true;
      document.getElementById("run").innerHTML = "<i class=\'fa fa-spinner fa-spin\'></i> Running...";
    ')
  }

  hide_overlay <- function() {
    shinyjs::runjs('
      document.getElementById("loading_overlay").style.display = "none";
      document.getElementById("run").disabled = false;
      document.getElementById("run").innerHTML = "<i class=\'fa fa-play\'></i> Run Simulation";
    ')
  }

  observeEvent(input$run, {
    message("[RUN] fired, old run_id=", run_id(), " cancel_id=", cancel_id())
    show_overlay()
    tabs_rendered(character(0))
    run_id(run_id() + 1)
    message("[RUN] new run_id=", run_id())
  })

  observeEvent(input$cancel, {
    message("[CANCEL] fired, run_id=", run_id(), " setting cancel_id=", run_id())
    cancel_id(run_id())
    hide_overlay()
  }, ignoreInit = TRUE)

  observeEvent(input$tabs, {
    if (run_id() > 0 && !is_cancelled() && !(input$tabs %in% tabs_rendered())) {
      show_overlay()
    }
  }, ignoreInit = TRUE)

  sim_data <- reactive({
    req(run_id() > 0, !is_cancelled())
    message("[sim_data] computing for run_id=", run_id())
    simulate_two_sample(n1 = input$n, n2 = input$n,
                        dist = input$dist, delta = input$delta)
  })

  perm_res <- reactive({
    req(run_id() > 0, !is_cancelled())
    message("[perm_res] computing for run_id=", run_id())
    dat <- sim_data()
    perm_test(x = dat$x, y = dat$y,
              B    = input$B,
              stat = input$stat,
              seed = input$seed + run_id())
  })

  mc_res <- reactive({
    req(run_id() > 0, !is_cancelled())
    message("[mc_res] computing for run_id=", run_id())
    mc_compare_tests(
      nsim  = input$nsim,
      n1    = input$n,
      n2    = input$n,
      dist  = input$dist,
      delta = input$delta,
      B     = 200,
      stat  = input$stat,
      seed  = input$seed + run_id()
    )
  })

  power_curve_res <- reactive({
    req(run_id() > 0, !is_cancelled())
    message("[power_curve_res] computing for run_id=", run_id())
    delta_seq <- seq(0, input$delta_max, length.out = 10)
    results   <- lapply(delta_seq, function(d) {
      res <- mc_compare_tests(
        nsim  = input$nsim_power,
        n1    = input$n,
        n2    = input$n,
        dist  = input$dist,
        delta = d,
        B     = 200,
        stat  = input$stat,
        seed  = input$seed
      )
      data.frame(
        delta            = d,
        t_test           = res$rejection_rates["t_test"],
        permutation_test = res$rejection_rates["permutation_test"]
      )
    })
    do.call(rbind, results)
  })

  robust_res <- reactive({
    req(run_id() > 0, !is_cancelled())
    message("[robust_res] computing for run_id=", run_id())
    dists <- c("normal", "exponential", "t")
    type1 <- lapply(dists, function(d) {
      res <- mc_compare_tests(
        nsim = input$nsim, n1 = input$n, n2 = input$n,
        dist = d, delta = 0, B = 200,
        stat = input$stat, seed = input$seed
      )
      data.frame(
        Distribution     = d,
        t_test           = round(res$rejection_rates["t_test"], 3),
        permutation_test = round(res$rejection_rates["permutation_test"], 3)
      )
    })
    power <- lapply(dists, function(d) {
      res <- mc_compare_tests(
        nsim = input$nsim, n1 = input$n, n2 = input$n,
        dist = d, delta = input$delta, B = 200,
        stat = input$stat, seed = input$seed
      )
      data.frame(
        Distribution     = d,
        t_test           = round(res$rejection_rates["t_test"], 3),
        permutation_test = round(res$rejection_rates["permutation_test"], 3)
      )
    })
    list(type1 = do.call(rbind, type1), power = do.call(rbind, power))
  })

  # ── Tab 1 Outputs ──────────────────────────────────────────────────────

  output$perm_plot <- renderPlot({
    req(!is_cancelled())
    message("[perm_plot] rendering, run_id=", run_id(), " is_cancelled=", is_cancelled())
    res <- perm_res()
    df  <- data.frame(stat = res$perm_stats)
    n_total <- input$n * 2 - 2

    p <- ggplot(df, aes(x = stat)) +
      geom_histogram(aes(y = after_stat(density)), bins = 35,
                     fill = input$col_hist, colour = "white", alpha = 0.85)

    if (input$show_tdist && input$stat == "t_stat") {
      x_range <- seq(min(df$stat) - 1, max(df$stat) + 1, length.out = 300)
      t_df    <- data.frame(x = x_range, y = dt(x_range, df = n_total))
      p <- p +
        geom_line(data = t_df, aes(x = x, y = y),
                  colour = "#E74C3C", linewidth = 1.2) +
        annotate("label", x = max(x_range) * 0.6, y = max(t_df$y) * 0.9,
                 label = paste0("t(df=", n_total, ")"),
                 fill = "#E74C3C", colour = "white", size = 3.5)
    }

    p <- p +
      geom_vline(xintercept =  res$obs_stat, colour = input$col_obs,
                 linewidth = 1.2, linetype = "solid") +
      geom_vline(xintercept = -res$obs_stat, colour = input$col_mirror,
                 linewidth = 1.2, linetype = "dashed") +
      annotate("label", x = res$obs_stat, y = Inf,
               label = paste("Observed =", round(res$obs_stat, 3)),
               vjust = 1.5, hjust = -0.05,
               fill = input$col_obs, colour = "white", size = 3.5) +
      labs(x = paste("Test statistic:", input$stat), y = "Density",
           title = "Permutation Null Distribution") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", colour = input$col_hist))

    tabs_rendered(union(tabs_rendered(), "Permutation Test"))
    hide_overlay()
    p
  })

  output$perm_pvalue <- renderText({
    res <- perm_res()
    paste0("Observed statistic: ", round(res$obs_stat, 4),
           "  |  Two-sided p-value: ", round(res$p_value, 4))
  })

  output$mc_table <- renderTable({
    rates <- mc_res()$rejection_rates
    data.frame(
      Test           = c("t-test", "Permutation test"),
      Rejection_Rate = round(rates, 3)
    )
  }, rownames = FALSE, striped = TRUE, hover = TRUE)

  output$mc_interpretation <- renderUI({
    rates  <- mc_res()$rejection_rates
    perm_r <- rates["permutation_test"]
    t_r    <- rates["t_test"]
    delta  <- input$delta
    alpha  <- 0.05

    if (delta == 0) {
      calib_perm <- ifelse(abs(perm_r - alpha) < 0.03, "well-calibrated", "slightly miscalibrated")
      calib_t    <- ifelse(abs(t_r - alpha) < 0.03,    "well-calibrated", "slightly miscalibrated")
      tagList(
        tags$b("Interpretation (\u03b4 = 0 \u2192 Type I error):"),
        tags$ul(
          tags$li(paste0("Permutation test rejects ", round(perm_r*100,1), "% \u2014 ", calib_perm, " (nominal \u03b1 = 5%)")),
          tags$li(paste0("t-test rejects ",           round(t_r*100,1),    "% \u2014 ", calib_t,    " (nominal \u03b1 = 5%)"))
        )
      )
    } else {
      tagList(
        tags$b(paste0("Interpretation (\u03b4 = ", delta, " \u2192 Power):")),
        tags$ul(
          tags$li(paste0("Permutation test power: ", round(perm_r*100,1), "%")),
          tags$li(paste0("t-test power: ",           round(t_r*100,1),    "%")),
          tags$li(if (perm_r >= t_r)
            "Permutation test has equal or greater power than t-test."
            else
              "t-test has greater power than permutation test in this setting.")
        )
      )
    }
  })

  output$mc_se <- renderPrint({
    rates <- mc_res()$rejection_rates
    se    <- sqrt(rates * (1 - rates) / input$nsim)
    cat("t-test:           ", round(se["t_test"],           4), "\n")
    cat("Permutation test: ", round(se["permutation_test"], 4), "\n")
  })

  # ── Tab 2: Power Curve ─────────────────────────────────────────────────

  output$power_curve_plot <- renderPlot({
    req(!is_cancelled())
    df <- power_curve_res()
    df_long <- rbind(
      data.frame(delta = df$delta, power = df$t_test,           Test = "t-test"),
      data.frame(delta = df$delta, power = df$permutation_test, Test = "Permutation test")
    )
    p <- ggplot(df_long, aes(x = delta, y = power, colour = Test, group = Test)) +
      geom_line(linewidth = 1.3) +
      geom_point(size = 2.5) +
      geom_hline(yintercept = 0.05, linetype = "dashed", colour = "gray50", linewidth = 0.8) +
      annotate("text", x = 0, y = 0.07, label = "\u03b1 = 0.05", colour = "gray50", hjust = 0, size = 3.5) +
      scale_colour_manual(values = c("t-test" = "#F8A04B", "Permutation test" = "#6C63FF")) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      labs(x = "Effect size (\u03b4)", y = "Rejection rate",
           title = paste0("Power Curve \u2014 ", input$dist, " distribution, n = ", input$n),
           colour = "Test") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", colour = "#6C63FF"), legend.position = "top")

    tabs_rendered(union(tabs_rendered(), "Power Curve"))
    hide_overlay()
    p
  })

  # ── Tab 3: Robustness ──────────────────────────────────────────────────

  output$robust_type1 <- renderTable({
    res <- robust_res()$type1
    colnames(res) <- c("Distribution", "t-test", "Permutation test")
    res
  }, striped = TRUE, hover = TRUE)

  output$robust_power_header <- renderUI({
    paste0("Power (\u03b4 = ", input$delta, ")")
  })

  output$robust_power <- renderTable({
    res <- robust_res()$power
    colnames(res) <- c("Distribution", "t-test", "Permutation test")
    res
  }, striped = TRUE, hover = TRUE)

  output$robust_plot <- renderPlot({
    req(!is_cancelled())
    type1 <- robust_res()$type1
    power <- robust_res()$power
    df <- rbind(
      data.frame(Distribution = type1$Distribution, t_test = type1$t_test,
                 permutation_test = type1$permutation_test, Scenario = "Type I Error (\u03b4=0)"),
      data.frame(Distribution = power$Distribution, t_test = power$t_test,
                 permutation_test = power$permutation_test,
                 Scenario = paste0("Power (\u03b4=", input$delta, ")"))
    )
    df_long <- rbind(
      data.frame(Distribution = df$Distribution, Scenario = df$Scenario, Rate = df$t_test,           Test = "t-test"),
      data.frame(Distribution = df$Distribution, Scenario = df$Scenario, Rate = df$permutation_test, Test = "Permutation test")
    )
    p <- ggplot(df_long, aes(x = Distribution, y = Rate, fill = Test, group = Test)) +
      geom_col(position = "dodge", alpha = 0.85, width = 0.6) +
      geom_hline(yintercept = 0.05, linetype = "dashed", colour = "gray40", linewidth = 0.8) +
      scale_fill_manual(values = c("t-test" = "#F8A04B", "Permutation test" = "#6C63FF")) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      facet_wrap(~Scenario, scales = "free_y") +
      labs(x = "Distribution", y = "Rejection rate",
           title = "Robustness: Rejection Rates Across Distributions", fill = "Test") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", colour = "#6C63FF"), legend.position = "top")

    tabs_rendered(union(tabs_rendered(), "Robustness Study"))
    hide_overlay()
    p
  })

  # ── Download ───────────────────────────────────────────────────────────

  output$download <- downloadHandler(
    filename = function() paste0("data_", input$dist, "_delta", input$delta, ".csv"),
    content  = function(file) {
      dat <- sim_data()
      write.csv(data.frame(x = dat$x, y = dat$y), file, row.names = FALSE)
    }
  )
}
