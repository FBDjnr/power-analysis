# Power Analysis
#
# An interactive illustration of significance level, Type II error, and power for
# a one-sample z test. Adjust the inputs and watch how the rejection region and
# the two error rates move.

library(shiny)

source("functions.R")

app_css <- "
body { background: #f7f8fa; }
.app-header { margin-bottom: 6px; }
.readout {
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
  margin: 0 0 16px 0;
}
.readout .stat {
  flex: 1 1 130px;
  background: #ffffff;
  border: 1px solid #dfe3e8;
  border-left: 4px solid #3d6b9e;
  border-radius: 8px;
  padding: 9px 12px;
}
.readout .stat.power { border-left-color: #2e8b57; }
.readout .stat.beta  { border-left-color: #b3446c; }
.readout .stat .label {
  display: block;
  font-size: 0.82rem;
  font-weight: 600;
  letter-spacing: 0.02em;
  color: #5b6875;
}
.readout .stat .value {
  font-size: 1.3rem;
  font-weight: 650;
  color: #1c2733;
}
.plot-title { margin: 4px 0 2px 0; }

/* Touch targets and spacing that work on a phone. Below the Bootstrap small
   breakpoint the sidebar stacks above the plots, so it also gets full width. */
@media (max-width: 767px) {
  .irs { margin-bottom: 4px; }
  .irs--shiny .irs-bar, .irs--shiny .irs-line { height: 10px; }
  .irs--shiny .irs-handle {
    width: 28px !important;
    height: 28px !important;
    top: 21px !important;
  }
  .irs--shiny .irs-handle > i:first-child { display: none; }
  .well { padding: 12px; }
  .readout .stat { flex: 1 1 44%; padding: 7px 10px; }
  .readout .stat .value { font-size: 1.12rem; }
  h2 { font-size: 1.45rem; }
  .radio label { padding: 3px 0; }
}
"

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML(app_css))
  ),

  div(
    class = "app-header",
    titlePanel("Power Analysis"),
    p(
      class = "text-muted",
      HTML("A one-sample z test of &mu;<sub>0</sub> = 40. Move the controls to see how the
           rejection region, the Type II error rate, and the power change together.")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      sliderInput("alpha", HTML("Significance Level (&alpha;):"),
                  min = 0.01, max = 0.25, value = 0.05, step = 0.01),
      sliderInput("mu1", HTML(paste0("Alternative Hypothesis Mean (&mu;", tags$sub("a"), "):")),
                  min = 15, max = 65, value = 30, step = 1),
      sliderInput("sigma", HTML("Population Standard Deviation (&sigma;):"),
                  min = 5, max = 15, value = 10, step = 0.5),
      sliderInput("n", HTML(paste0("Sample Size (", tags$em("n"), "):")),
                  min = 1, max = 100, value = 5, step = 1),
      radioButtons("direction", "Direction of Test",
                   choices = c("Lower Tail" = "LTail",
                               "Upper Tail" = "UTail",
                               "Two-Tailed" = "TwoTail"),
                   selected = "LTail")
    ),

    mainPanel(
      width = 8,
      uiOutput("readout"),
      h4("Alternative Distribution", class = "plot-title", align = "center"),
      plotOutput("H1Plot", height = "auto"),
      h4("Null (Comparison) Distribution", class = "plot-title", align = "center"),
      plotOutput("H0Plot", height = "auto")
    )
  )
)

server <- function(input, output, session) {

  # Dragging a slider fires on every step. Debouncing collapses a drag into one
  # redraw at the end of it instead of one per intermediate value.
  settings <- debounce(reactive({
    list(alpha = input$alpha, mu1 = input$mu1, sigma = input$sigma,
         n = input$n, direction = input$direction)
  }), 120)

  # Computed once per change and shared by both panels and the readout, rather
  # than being recalculated inside each output.
  quantities <- reactive({
    s <- settings()
    test_quantities(s$alpha, s$mu1, s$sigma, s$n, s$direction)
  })

  # Panels get shorter and their text smaller as the viewport narrows, so the
  # whole app stays usable on a phone.
  panel_height <- function(width) {
    if (is.null(width)) return(380)
    max(230, min(400, round(width * 0.58)))
  }
  panel_scale <- function(width) {
    if (is.null(width)) return(1)
    max(0.62, min(1, width / 640))
  }

  output$readout <- renderUI({
    q <- quantities()
    power <- 1 - q$type_ii
    # The critical value is quoted on the z scale under the null, which is what the
    # test is actually carried out on and does not move with sigma or n. The two
    # tailed cuts are symmetric about zero, so one magnitude states both.
    cut_label <- if (identical(q$direction, "TwoTail")) {
      HTML(paste0("&plusmn;", format(abs(q$z0_u), nsmall = 2)))
    } else {
      format(q$z0, nsmall = 2)
    }

    div(
      class = "readout",
      div(class = "stat", span(class = "label", HTML("Significance &alpha;")),
          span(class = "value", q$alpha)),
      div(class = "stat beta", span(class = "label", HTML("Type II Error &beta;")),
          span(class = "value", q$type_ii)),
      div(class = "stat power", span(class = "label", HTML("Power 1 &minus; &beta;")),
          span(class = "value", power)),
      div(class = "stat", span(class = "label", HTML("Critical Value (<em>z</em>)")),
          span(class = "value", cut_label))
    )
  })

  output$H0Plot <- renderPlot(
    null_plot(quantities(), panel_scale(session$clientData$output_H0Plot_width)),
    height = function() panel_height(session$clientData$output_H0Plot_width)
  )

  output$H1Plot <- renderPlot(
    alternative_plot(quantities(), panel_scale(session$clientData$output_H1Plot_width)),
    height = function() panel_height(session$clientData$output_H1Plot_width)
  )
}

shinyApp(ui = ui, server = server)
