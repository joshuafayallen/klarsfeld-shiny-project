library(arrow)
library(htmltools)
library(reactable)
library(bslib)
library(tibble)
library(mapgl)
library(sf)
library(shiny)


server <- function(input, output, session) {
  departments = reactive({
    st_read(here::here('shape-files', 'DEPARTEMENTS_1940.shp')) |>
      st_transform(4326)
  })

  memorial_map_data = reactive({
    read_sf(
      'https://github.com/joshuafayallen/diss-data-public/releases/download/1.0.0/klarsfeld-memorial.geojson'
    )
  })

  memorial_table_data = reactive({
    read_parquet(here::here('data', 'table-data.parquet'))
  })
  # Map output
  output$map <- renderMaplibre({
    mapboxgl(bounds = departments()) |>
      add_circle_layer(
        id = 'points',
        source = memorial_map_data(),
        tooltip = 'tool_tip_labs',
        circle_opacity = 0.5,
        circle_radius = 2
      )
  })

  # Table output with link formatting
  output$table <- renderReactable({
    # Define which columns contain links (excluding Yad Vashem which gets special treatment)
    data = memorial_table_data()
    link_columns <- c(
      "Birth Place Link",
      "Other Sources",
      "MDFJF Entry",
      "Permalink"
    )

    # Create column definitions
    column_defs <- list(
      Fate = colDef(
        filterInput = function(values, name) {
          tags$select(
            onchange = sprintf(
              "Reactable.setFilter('table', '%s', event.target.value || undefined)",
              name
            ),
            tags$option(value = "", "All"),
            lapply(unique(values), tags$option),
            "aria-label" = sprintf("Filter %s", name),
            style = "width: 100%; height: 28px;"
          )
        }
      )
    )

    # Add special treatment for Yad Vashem Page Of Testimony column
    if ("Yad Vashem Page Of Testimony" %in% colnames(memorial_table_data)) {
      column_defs[[
        "Yad Vashem Page Of Testimony"
      ]] <- create_testimony_column_def("Yad Vashem Page Of Testimony")
    }

    # Add link column definitions for other link columns
    for (col in link_columns) {
      if (col %in% colnames(memorial_table_data)) {
        column_defs[[col]] <- create_link_column_def(col)
      }
    }

    reactable(
      data,
      defaultSorted = c('Surname', 'Given Name'),
      defaultSortOrder = 'asc',
      filterable = TRUE,
      selection = 'multiple',
      columns = column_defs,
      defaultPageSize = 20
    )
  })
  # Download handler - updated to use parquet format
  output$download1 <- downloadHandler(
    filename = function() {
      paste("memorial_data_", Sys.Date(), ".geojson", sep = "")
    },
    content = function(file) {
      # Write data as parquet file using arrow package
      sf::write_sf(memorial_table_data, file)
    }
  )
}
