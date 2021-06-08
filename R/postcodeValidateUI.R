#' Shiny module for address validation with Postcode API
#' @param id Shiny input id
#' @param input
#' @param output
#' @param session
#' @param extra_fields If TRUE, adds extra input fields 'huisletter' and 'huisnummertoevoeging',
#' of which neither is used in address validation but will be added to the output.
#' @details See \code{\link{postcode_validate}} on how to set the API key.
#' @export
#' @examples
#' @rdname postcodeValidate
#' \dontrun{
#'   library(shiny)
#'
#' ui <- fluidPage(
#'   useShinyjs(),
#'   postcodeValidateUI("test"),
#'   tags$hr(),
#'   verbatimTextOutput("txt_out")
#'
#' )
#'
#' server <- function(input, output, session) {
#'   out <- callModule(postcodeValidate, "test")
#'
#'   output$txt_out <- renderPrint({
#'     out()
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @importFrom shiny textInput numericInput NS actionButton
#' @importFrom shiny updateTextInput observeEvent
#' @importFrom shinyjs disabled
postcodeValidateUI <- function(id, extra_fields = TRUE){


  ns <- NS(id)

  out <- tagList(
    side_by_side(
      textInput(ns("txt_postcode"), "Postcode", value = '', width = 100, placeholder = "1234AB"),
      numericInput(ns("num_huisnummer"), "Huisnummer", value = '', width = 100)
    ),
    tags$br(),
    actionButton(ns("btn_validate"), "Zoeken", class = "btn-success", icon = icon("map-marker")),

    shinyjs::disabled(
      textInput(ns("txt_straatnaam"), "Straat"),
      textInput(ns("txt_woonplaats"), "Woonplaats")
    )

  )

  if(extra_fields){
    out <- c(out, list(
      side_by_side(
        textInput(ns("txt_huisletter"), "Huisletter", value = '', width = 100, placeholder = "A"),
        textInput(ns("txt_huisnummertoevoeging"), "Huisnummertoevoeging", value = '', width = 100, placeholder = "101")
      )
    ))
  }

  out

}


#' @rdname postcodeValidate
#' @export
postcodeValidate <- function(input, output, session){

  validate_result <- reactiveVal()

  observeEvent(input$btn_validate, {

    req(input$txt_postcode)
    req(input$num_huisnummer)

    out <- postcode_validate(input$txt_postcode, input$num_huisnummer)

    validate_result(out)
  })


  observeEvent(validate_result(), {

    out <- validate_result()
    updateTextInput(session, "txt_straatnaam", value = out$street)
    updateTextInput(session, "txt_woonplaats", value = out$city)
  })


  address_out <- reactive({

    lis <- validate_result()
    if(is.null(lis)){
      return(NULL)
    } else {
      c(lis, list(huisletter = input$txt_huisletter,
                  huisnummertoevoeging = input$txt_huisnummertoevoeging))
    }


  })

  return(address_out)

}

