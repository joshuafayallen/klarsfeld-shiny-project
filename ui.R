library(arrow)
library(htmltools)
library(reactable)
library(bslib)
library(mapgl)
library(tibble)
library(sf)
library(shiny)

download_button <- function(
  id,
  link = "https://github.com/joshuafayallen/diss-data-public/releases/download/1.0.0/klarsfeld-memorial.geojson",
  label = "Download the Data"
) {
  tags$button(
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}


filter_fun = function(values, name) {
  tags$select(
    onchange = sprintf(
      "Reactable.setFilter('fate-select', '%s', event.target.value || undefined)",
      name
    ),
    # "All" has an empty value to clear the filter, and is the default option
    tags$option(value = "", "All"),
    lapply(unique(values), tags$option),
    "aria-label" = sprintf("Filter %s", name),
    style = "width: 100%; height: 28px;"
  )
}

shorten_url <- function(url, max_length = 30) {
  if (nchar(url) <= max_length) {
    return(url)
  }

  # Extract domain from URL
  domain <- gsub("^https?://", "", url)
  domain <- gsub("/.*$", "", domain)

  # Create shortened version
  paste0(domain, "...")
}

# Function to create HTML links from comma-separated URLs
create_links_html <- function(links_string) {
  if (is.na(links_string) || links_string == "") {
    return("")
  }

  # Split by comma and trim whitespace
  urls <- trimws(strsplit(links_string, ",")[[1]])

  # Create HTML links
  html_links <- sapply(urls, function(url) {
    shortened <- shorten_url(url)
    sprintf(
      '<a href="%s" target="_blank" style="margin-right: 8px;">%s</a>',
      url,
      shortened
    )
  })

  # Join with line breaks for multiple links
  paste(html_links, collapse = "<br/>")
}

create_link_column_def <- function(column_name) {
  colDef(
    name = column_name,
    cell = function(value) {
      create_links_html(value)
    },
    html = TRUE,
    minWidth = 200
  )
}

create_testimony_links_html <- function(links_string) {
  if (is.na(links_string) || links_string == "") {
    return("")
  }

  # Split by comma and trim whitespace
  urls <- trimws(strsplit(links_string, ",")[[1]])

  # Create HTML links with custom text
  html_links <- sapply(urls, function(url) {
    sprintf(
      '<a href="%s" target="_blank" style="margin-right: 8px; color: #0066cc; text-decoration: none;">Look for Page of Testimony</a>',
      url
    )
  })

  # Join with line breaks for multiple links
  paste(html_links, collapse = "<br/>")
}

create_testimony_column_def <- function(column_name) {
  colDef(
    name = column_name,
    cell = function(value) {
      create_testimony_links_html(value)
    },
    html = TRUE,
    minWidth = 200
  )
}

ui <- fluidPage(
  theme = bs_theme(),
  tabsetPanel(
    tabPanel(
      'Search the Memorial',
      reactableOutput('table')
    ),
    tabPanel(
      "Map of The Deported",
      card(
        full_screen = TRUE,
        mapboxglOutput("map")
      )
    ),
    tabPanel(card(
      card_header("Download Memorial Data"),
      card_body(
        p("Download the complete memorial dataset."),
        br(),
        downloadButton(
          'download1',
          'Download Data as a Geojson',
          class = "btn-primary",
          icon = icon("download")
        )
      )
    ))
  )
)
