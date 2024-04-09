# my_modal <- function(
#     id,
#     content) {
#
#
#
#   tags$div(
#     id = id,
#     class = "modal fade epic",
#     `data-bs-backdrop` = "static",
#     `data-bs-keyboard` = "false",
#     tabindex = "-1",
#     tags$div(
#       class = "modal-dialog modal-dialog-centered",
#       tags$div(
#         class = "modal-content",
#         tags$div(
#           class = "modal-body",
#           content
#         ),
#
#         tags$div(
#           class = "modal-footer",
#           actionButton("dismiss_modal", "Dismiss", class = "btn btn-default", `data-dismiss` = "modal")
#         )
#       )
#     ),
#
#     tags$script(
#       HTML(
#         "if (window.bootstrap && !window.bootstrap.Modal.VERSION.match(/^4\\./)) {
#             var modal = new bootstrap.Modal(document.getElementById('my_modal'));
#             modal.show();
#           } else {
#             $('#my_modal').modal('show').focus();
#           };
#
#        document.getElementById('dismiss_modal').addEventListener('click', function() {
#       var myModalEl = document.getElementById('my_modal');
#       var modal = bootstrap.Modal.getInstance(myModalEl);
#       modal.hide();
#     });"
#       )
#     )
#
#   )
#
#
#
# }
my_modal <- function(id, content) {
  dismiss_button_id <- paste(id, "dismiss", sep = "_")  # Creating a unique ID for the dismiss button

  tags$div(
    id = id,
    class = "modal fade epic",
    `data-bs-backdrop` = "static",
    `data-bs-keyboard` = "false",
    tabindex = "-1",
    tags$div(
      class = "modal-dialog modal-dialog-centered",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-body",
          content
        ),
        tags$div(
          class = "modal-footer",
          actionButton(dismiss_button_id, "Dismiss", class = "btn btn-default", `data-bs-dismiss` = "modal")
        )
      )
    ),
    tags$script(
      HTML(
        sprintf("if (window.bootstrap && !window.bootstrap.Modal.VERSION.match(/^4\\./)) {
                  var modal = new bootstrap.Modal(document.getElementById('%s'));
                  modal.show();
                } else {
                  $('#%s').modal('show').focus();
                };

                document.getElementById('%s').addEventListener('click', function() {
                  var myModalEl = document.getElementById('%s');
                  var modal = bootstrap.Modal.getInstance(myModalEl);
                  modal.hide();
                });", id, id, dismiss_button_id, id)
      )
    )
  )
}
