ui <- dashboardPage( skin = "black",
   dashboardHeader(title = "Datacrunch 2015"),
   dashboardSidebar(tags$head(tags$style("#plot1{height:calc(100vh - 125px) !important;}")),
                    includeCSS("styles.css"),
                    sidebarMenu(id = "sidebarmenu",
                    menuItem("School Boundary", tabName = "schoolMenu",  icon = icon("bus", lib="font-awesome")),
                    conditionalPanel("input.sidebarmenu === 'schoolMenu'",
                    selectInput("school", "School", schools$school.name),
                    checkboxGroupInput("checkboxGroup",
                    "Select An Option",
                    unique(places$place.type)[unique(places$place.type) != "Schools"],
                    inline = FALSE,
                    selected = "Convenience Stores")),
                    menuItem("School District", tabName = "countyMenu", icon = icon("bank", lib = "font-awesome")),
                    conditionalPanel("input.sidebarmenu === 'countyMenu'",
                                     selectInput("district", "District",
                                                 unique(district.shapes$name)),
                                     checkboxGroupInput("checkboxGroup2",
                                                        "Select An Option",
                                                        unique(places$place.type),
                                                        inline = FALSE,
                                                        selected = "Convenience Stores")

                                     ),
                    menuItem("About", tabName = "about", icon = icon("info-circle", lib = "font-awesome"))
                    )),
   dashboardBody(
      tabItems(
      # First tab content
      tabItem(tabName = "schoolMenu",
     # Boxes need to be put in a row (or column)
     fluidRow(
       box(leafletOutput("plot1"), width=12)

     ),
       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
         draggable = FALSE, top = 85, left = "auto", right = 25, bottom = "auto",
         width = 330, height = "auto",

         htmlOutput('schoolStats'),

         plotOutput(outputId="lunchPie", height = 265, width = 265)
       )
     ),
     tabItem(tabName = "countyMenu",
       fluidRow(
         box(leafletOutput("plot2"), width=12)

       )
     ),
    tabItem(tabName = "about",
            h1("NC Food Resource Exploration Map"),

            h2("Main Objective"),

            p("We wanted to provide a tool for organizations to explore the resources available and the demand for food assistance. The map we created will help organizations start to understand what areas have the biggest mismatch and need the most attention."),

            h2("Background"),

            p("Partnering with the United Way of the Greater Triangle, NC Data4Good is using data to explore questions about food insecurity in Durham, Johnston, Orange and Wake Counties."),

            h2("Insights"),
            ##TODO: Put Insights Here and in ABOUT.txt
            p("Our maps shows the locations of: schools, feeding sites, churches, convenience stores, grocery stores, farmers markets, food pantries, SNAP retailers and bus stops. The map breaks the counties down by school district and school boundaries for varying levels of granularity. It also provides basic summary statistics for each school including: total number of students, number of students on free or reduced lunch, and a performance measure of the school."),

            h2("Description of Files"),
            p(
              span("This is a "),
              a(href="http://shiny.rstudio.com/","Shiny App"),
              span("written in R. The full code can be found in GitHub (below).  The files included are")
            ),

            p(
              downloadLink("placesDownload","places.rda"),
              span(" - combined orginal data sources")
            ),

            p(
              downloadLink("schoolsDownload","schools.rda"),
              span(" - combined data file including census data and school achievement data.")
            ),
            p("dist-mat.rda  - distances between all the places and all the other places."),
            p("ui.R - Shiny App user interface."),
            p("global.R - Initialization file for Shiny"),
            p("server.R - Code that create the visualizations and graphs."),
            p("tidy.R - Processes raw data to create schools.rda."),
            p("styles.css - A few custom stylesheet definitions."),
            h2("Data Sources"),
            p(
              span("The data sources were provided by "),
              a(href="http://ncdata4good.github.io/DataCrunch2015/","NCData4Good"),
              span("and include the following: ")
            ),
            p(
              a(href="https://raw.githubusercontent.com/Msanghi/NCData4Good/master/Export.csv","Farmer's Markets"),
              br(),
              a(href="https://raw.githubusercontent.com/DrZeydy/Data4Good/gh-pages/SNAP-NC.csv", "SNAP Retailers"),
              br(),
              a(href="https://raw.githubusercontent.com/NCData4Good/DataCrunch2015/gh-pages/NCdata.csv","Schools, Churches, and Community places"),
              br(),
              a(href="http://nces.ed.gov/surveys/sdds/sabs/","Schools Boundaries"),
              br(),
              a(href="ftp://ftp2.census.gov//geo/tiger/TIGER2015/UNSD/tl_2015_37_unsd.zip", "School District Boundaries Shapefile"),
              br(),
              a(href="https://github.com/DrZeydy/Data4Good/blob/gh-pages/busstops.rds", "Bus Stops")

            ),

            h2("Future Work"),
            p("We'd like to continue to build this tool as a way to visualize and explore data around hunger.  Some of the features that may be useful to include in future versions of this tool include:"),
            p("- Expand the scope to include data from across North Carolina"),
            p("- Include census boundaries as an option in addition to school boundaries and district boundaries"),
            p("- Add district level stats"),
            p("- Clean and refactor code"),
            p("- Collaborate with United Way to determine most useful stats to display"),

            h3("Additional Data Desired"),
            p("To further increase the effectiveness of this tool, we'd like to have more data about:"),
            p("- The number of meals prepared for each feeding site"),
            p("- More granular data on food insecurity"),

            h2("Contibutors"),
            p(
              span("Jordan Meyer - "),
              a(href="mailto:jordan.meyer@rittmanmead.com","jordan.meyer@rittmanmead.com")
            ),
            p(
              span("Joe Elliott - "),
              a(href="mailto:joe.elliott@joulebug.com","joe.elliott@joulebug.com")
            ),
            p(
              span("Iain Carmichel - "),
              a(href="idcarm@live.unc.edu","idcarm@live.unc.edu")
            ),

            h2("GitHub Link"),
            strong(
              a(href = "http://github.com/NCData4Good/UWchallenge", "Link to GitHub")
            )
    )
    )


   )
   )
