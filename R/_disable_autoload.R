# Shiny automatically runs every file inside a folder called R/ when an app starts.
# Our R/ folder holds the analysis scripts (01_... 08_...), which must NOT run
# every time the dashboard opens, so autoloading is switched off here.
options(shiny.autoload.r = FALSE)
