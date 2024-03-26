#' clickable_card
#'
#' Description
#'
#' @param inputid [character] of [length] 1. The id of the element. Can be wrapped in [NS].
#' @param outputval [character] of [length] 1. The outputted value when the card is clicked.
#' @param ... parameters passed into [bslib::card()]-funciton.
#'
#'
#' @return The return value, if any, from executing the utility.
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
        style = "transition: box-shadow 0.3s ease-in-out; cursor: pointer; user-select: none;",
        ... = ...,
        class = "hover-elevate"
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
    ),

    # set the style
    # of the hover-elevate class
    # TODO: Change to something
    # else to avoid overwriting
    # other tags
    tags$style(
      HTML("
      .hover-elevate:hover {
        box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
        transform: scale(1.05);
      }
    ")
    )
    # tagList end;
  )
}

# clickable_card <- function(
    #     inputid,
#     outputval,
#     ...) {
#
#
#   tagList(
#     div(
#       id = inputid,
#       #id = "clickableCard",
#       bslib::card(
#         style = "transition: box-shadow 0.3s ease-in-out; cursor: pointer; user-select: none; transition: transform 0.5s ease;",
#         ... = ...,
#         class = "hover-elevate"
#
#       )
#     ),
#     tags$script(
#       HTML(
#         sprintf("
#       $(document).ready(function() {
#         $('#%s').on('click', function() {
#           Shiny.setInputValue('%s', '%s', {priority: 'event'});
#         });
#       });
#     ",
#     inputid,
#     #"clickableCard",
#     # Has to be wrapped in
#     # ns
#     inputid,
#     outputval
#     # ,
#     # id
#         )
#       )
#     ),
#     tags$style(HTML("
#
#     .hover-elevate:hover {
#       box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
#     }
#   "))
#   )
# }

