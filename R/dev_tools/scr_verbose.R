# script: scr_verbose
# date: 2023-06-17
# author: Serkan Korkmaz, serkor1@duck.com
# objective: These functions generate various buggers, warnings and infos
# to track the inputs, outputs and alike in the application
# to debug
# script start;

msg_startup <- function(version, dev_mode, verbose, plot_color, segment_color) {
  
  symbol <- cli::symbol
  
  # header;
  cli::cli_rule(
    left = cli::col_magenta(
      'Powering application'
    ),
    right = paste(
        cli::col_magenta('Version:')
        ,
      cli::col_white(
        '{version}'
        )
    )
  )

  
  # header content;
  
  # Developper mode;
  cli::cli_text(
    cli::style_bold(
      cli::col_blue(
        '{symbol$info} Developper mode: '
      )
    ),
    cli::col_yellow('{dev_mode}')
  )
  
  # Verbose mode;
  cli::cli_text(
    cli::style_bold(
      cli::col_blue(
        '{symbol$info} Verbose mode: '
      )
    ),
    cli::col_yellow('{verbose}')
  )
  
  # plot color;
  cli::cli_text(
    cli::style_bold(
      cli::col_blue(
        '{symbol$info} Plot color: '
      )
    ),
    cli::col_white('{plot_color}')
  )
  
  
  # segment color;
  cli::cli_text(
    cli::style_bold(
      cli::col_blue(
        '{symbol$info} Segment color: '
      )
    ),
    cli::col_white('{segment_color}')
  )
}

# script end;

