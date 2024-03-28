# script: cards
# author: Serkan Korkmaz, serkor1@duck.com
# date: 2024-03-27
# objective: Generate a set of
# predefined cards
# script start;

#' Create an elevated and clickable card
#'
#' An elevated card that is clickable.
#'
#' @param inputid [character] of [length] 1. The id of the element. Can be wrapped in [NS].
#' @param outputval [character] of [length] 1. The outputted value when the card is clicked.
#' @param ... parameters passed into [bslib::card()]-function.
#'
#'
#' @returns A clickable [bslib::card()] with [class] `clickable-card`
clickable_card <- function(
    inputid,
    outputval,
    ...) {


  tagList(
    # tagList start;

    # define the clickable
    # card
    div(
      id = inputid,
      bslib::card(
        ... = ...,
        # NOTE: do NOT change
        # this class. It will break otherwise.
        class = "clickable-card"
      )
    ),

    # define javascript
    # on clicks
    tags$script(
      HTML(
        sprintf("
          $(document).ready(function() {
            $('#%s').on('click', function() {
              var card = $(this);
              card.fadeOut(250, function() {
                Shiny.setInputValue('%s', '%s', {priority: 'event'});
                // Optional: remove the card from DOM after fadeOut
                card.remove();
              });
            });
          });
        ",
        # define input id
        # in the script
        inputid,
        inputid,

        # output value
        # when clicked
        outputval
        )
      )
    )
  )
}


#' Title
#'
#' @param header
#' @param footer
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
options_card <- function(
    header = "header",
    footer = NULL,
    ...) {




  tagList(
    bslib::card(
      class = "options-card",
      bslib::card_header(header),
      bslib::card_body(
        class = "options-card-body",
        ...
      ),
      if (!is.null(footer)){
        bslib::card_footer(
          footer
        )
      }

    )
  )

}



# script end;

