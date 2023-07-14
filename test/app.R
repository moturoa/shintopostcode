


# Usage:
# Zet de API key 1 keer als option (zie 1Password)
#options(postcode_api_key = "<<KEY>>")

devtools::load_all()

ui <- fluidPage(style = "width: 600px; padding: 50px; margin: auto; border: 1px solid black;",
  postcodeValidateUI("postcode"),
  tags$hr(),
  verbatimTextOutput("txt_out")
)

server <- function(input, output, session) {

  out <- callModule(postcodeValidate, "postcode")

  output$txt_out <- renderPrint({
    out()
  })
}

shinyApp(ui, server)

