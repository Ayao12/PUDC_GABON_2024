#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "PUDC GABON 2024"),

  ## Sidebar content
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$script('
      // Define function to set height of "map" and "map_container"
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height - 30;

        $("#map_container").height(boxHeight);
        $("#map").height(boxHeight - 20);
      };

      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });

      // Refresh the box height on every window resize event
      $(window).on("resize", function(){
        setHeight();
      });
    ')),
    fluidRow(
    box(width = 6,
        #p("PUDC GABON 2024"),
        leafletOutput(outputId = "map")
    ),
    box(width = 6,
        h2(htmlOutput("text1",
                      style = "text-align:center;color:#ff8033")),
        h3(htmlOutput("text3",
                      style = "text-align:center;color:#FF0000")),

        actionButton(inputId ="Previous", label = icon("arrow-left")),
        actionButton(inputId ="Next", label = icon("arrow-right")),

        tabsetPanel(
          id = 'dataset',
          tabPanel("COMPOSANTE 1", DT::dataTableOutput("tab_comp1"), width = "200%"),
          tabPanel("COMPOSANTE 2", DT::dataTableOutput("tab_comp2")),
          tabPanel("COMPOSANTE 3", DT::dataTableOutput("tab_comp3")),
          tabPanel("COMPOSANTE 4", DT::dataTableOutput("tab_comp4"))
        ),


        h3(htmlOutput("text2",
                      style = "text-align:center;color:#6495ED")),


        DT::DTOutput("table1"),



)
        #
        # div(style = "display: inline-block;vertical-align:center;",
        #     actionButton("left", label = "<<")),
        # div(style = "display: inline-block;vertical-align:center;",
        #     sliderInput("obs", "",
        #                 min = 0, max = 1000, value = 500
        #     )),
        # div(style = "display: inline-block;vertical-align:center;",
        #     actionButton("right", label = ">>")),
        #
        # h3(htmlOutput("text3",
        #               style = "text-align:center;color:#6495ED")),
       # DT::DTOutput("table2"),


    )
  ),
  title = "Interactive Maps"
  # reactableOutput("table")
)
