# script: scr_renderTabs
# date: 2024-03-29
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Demonstrate the use
# of tabs-function
# script start;

# Create tabs with
# some random content
create_tabs(
  X = 1:10,
  fn = function(x) {

    p(
      paste(
        "content of tab:", x
      )
    )

  },
  bslib::nav_item(
    actionButton(
      inputId = "create_tabs",
      label = "Button"
    )
  )
)


# script end;
