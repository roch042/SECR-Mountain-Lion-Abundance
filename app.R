require(shiny); require(tidyverse); require(sf); require(adehabitatHR); require(slickR); require(leaflet); require(viridis); require(shinycssloaders)

load("data/mtn_camera_data.RData")
load("data/EPMU.RData")
load("data/spatial_objects.RData")

source("code/fnc_camera_collar_map.R")

spinner_color <- "black"

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Bear Lake Mountain Lions"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            width = 3,
            br(),
            uiOutput("sel_Date"),
            uiOutput("sel_collarloc"),
            conditionalPanel(
                condition = "input.flt_collarloc == 1",
                uiOutput("sel_homerange"),
                uiOutput("sel_dailyloc"),
                uiOutput("sel_spec_animal"),
                uiOutput("sel_locdate")
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            width = 9,
            tabsetPanel(
                tabPanel(
                    title = strong("Map"),
                    column(3,
                           br(),
                           wellPanel(
                               h4(strong("Camera Summary")),
                               br(),
                               h5(strong("All Mountain Lions:")),
                               uiOutput("mtlion_summary"),
                               br(),
                               h5(strong("Marked Mountain Lions:")),
                               uiOutput("marked_mtlion_summary")
                           ),
                           br(),
                           h5(strong("Home Range Maps:")),
                           h5(a("seasonal homeranges",target="_blank",href="kde_mountain_lions_by_season.pdf")),
                           h5(a("winter homerange overlap",target="_blank",href="winter_kde_lions_overlap.pdf")),
                           h5(a("summer homerange overlap",target="_blank",href="summer_kde_lions_overlap.pdf"))
                    ),
                    column(9,
                           conditionalPanel(
                               condition = 'input.flt_dailyloc == null || input.flt_dailyloc != "Daily Locations"',
                               leafletOutput("map", height="70vh") %>% withSpinner(color=spinner_color)
                           ),
                           leafletOutput("map2",height="70vh") %>% withSpinner(color=spinner_color),
                           br(),
                           fluidRow(
                               column(9),
                               column(2,
                                      downloadButton("download_map",label="Download Map")
                               ),
                               column(1)
                           )
                    )
                ),
                tabPanel(
                    title = strong("Pictures"),
                    fluidRow(
                        br(),
                        uiOutput("sel_camID"),
                        br(),
                        column(2),
                        column(8,
                               slickR::slickROutput("slickr_camera") %>% withSpinner(color=spinner_color)
                        ),
                        column(2)
                    )
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    results <- reactive({
        
        req(input$flt_Date)
        
        fnc_camera_collar_map(
            start_date = input$flt_Date[1],
            end_date = input$flt_Date[2],
            spec_animal = spec_animal(),
            homerange = ifelse(input$flt_collarloc == TRUE, homerange(), NA),
            collarloc = ifelse(isTruthy(input$flt_collarloc),input$flt_collarloc, FALSE),
            min_circle_radius = 1,
            max_circle_radius = 5,
            DAT = DAT,
            mtlion_index = mtlion_index,
            mtlion_pics = mtlion_pics,
            sf_ID_gmu = sf_ID_gmu,
            sf_mtlion_pic_grid = sf_mtlion_pic_grid,
            sf_mtlion_marked_pic_grid = sf_mtlion_marked_pic_grid,
            sf_pic_grid = sf_pic_grid,
            input = input
        )
        
    })
    
    output$map <- renderLeaflet({
        req(results())
        return(results()$device.map)
    })
    
    output$map2 <- renderLeaflet({
        # req(results())
        req(input$flt_dailyloc == "Daily Locations")
        results()$device.map_grid %>%
            addLayersControl(
                baseGroups = c("Default","Satellite"),
                overlayGroups = c("GMUs","Deployed Cameras","Mountain Lion Camera","Marked Mountain Lion Camera"),
                options = layersControlOptions(collapsed = FALSE)
            )
    })
    
    observe({
        
        req(input$flt_dailyloc == "Daily Locations")
        req(results())
        
        device.map <- leafletProxy("map2")
        
        flt_animal_ids <- results()$flt_animal_ids
        flt_collar_sf_gmu <- results()$flt_collar_sf_gmu
        
        color_group_domain <- flt_collar_sf_gmu$label
        
        pal_color_choice <- colorFactor(palette = "viridis",
                                        domain = color_group_domain,
                                        ordered = FALSE,
                                        na.color = "transparent",
                                        alpha = FALSE,
                                        reverse = FALSE)
        

        layer.group <- list()
        
        # i <- 1
        
        for(i in 1:length(flt_animal_ids)) {
            
            df_all <- flt_collar_sf_gmu %>%
                dplyr::filter(`Animal ID` == flt_animal_ids[i])  %>%
                dplyr::arrange(`Location Date (GMT)`)
            
            df <- df_all %>%
                dplyr::filter(lubridate::date(`Location Date (GMT)`) %in% lubridate::date(locdate()))
            
            coords <- sf::st_coordinates(df)
            
            group_label <- df_all$label
            
            crcrads <- seq(results()$min_circle_radius,results()$max_circle_radius,length.out=nrow(df))
            
            device.map <- device.map %>%
                clearGroup(group_label)
            
            if(nrow(df) > 0){
                
                
                
                device.map <- device.map %>%
                    
                    # add polygons of homeranges
                    addPolylines(data=df,
                                 lng = coords[,"X"], lat = coords[,"Y"],
                                 stroke=TRUE,
                                 color = ~pal_color_choice(label),
                                 weight = 1,
                                 opacity = .6,
                                 fillOpacity = .6,
                                 group = group_label) %>%
                    
                    addCircleMarkers(data=df,
                                     lng = coords[,"X"], lat = coords[,"Y"],
                                     color = ~pal_color_choice(label),
                                     radius = crcrads,
                                     opacity = .6,
                                     fillOpacity = .6,
                                     group = group_label,
                                     popup = paste(sep = "<br>",
                                                   paste("<b>Animal ID:</b> ", flt_animal_ids[i]),
                                                   paste("<b>Sex:</b> ", df$Sex),
                                                   paste("<b>Age Class (Capture):</b> ", df$`Age Class (Capture)`),
                                                   paste("<b>Capture Date:</b> ", df$`Capture Date`),
                                                   paste("<b>Location Date (GMT):</b> ", df$`Location Date (GMT)`)))
                
                layer.group <- c(layer.group, as.character(group_label))
                
            }
        }
        
        device.map <- device.map %>%
            addLayersControl(
                baseGroups = c("Default","Satellite"),
                overlayGroups = c("GMUs","Deployed Cameras","Mountain Lion Camera","Marked Mountain Lion Camera",layer.group),
                options = layersControlOptions(collapsed = FALSE)
            ) %>%
            hideGroup(c("Mountain Lion Camera", "Marked Mountain Lion Camera"))
        
    })
    
    #___ download_map ####
    output$download_map <- downloadHandler(
        filename = function() {
            paste("map-", Sys.Date(), ".html", sep="")
        },
        content = function(file) {
            saveWidget(
                widget = map()
                , file = file
            )
        }
    )
    
    # __ sel_Date ####
    output$sel_Date <- renderUI({
        start_date <- min(sf_pic_grid$first_pic, na.rm=T)
        end_date <- max(sf_pic_grid$last_pic, na.rm=T)
        dateRangeInput("flt_Date",
                       "Camera Grid Date Range",
                       start = start_date,
                       end = end_date,
                       min = start_date,
                       max = end_date)   
    })
    
    # __ sel_collarloc ####
    output$sel_collarloc <- renderUI({
        checkboxInput("flt_collarloc",
                      strong("Add Collar Data"),
                      value = FALSE)
    })
    
    #___ sel_homerange ####
    output$sel_homerange <- renderUI({
        req(input$flt_collarloc == TRUE)
        choices <- c("Kernel Density Estimate","Location Data")
        radioButtons(
            "flt_homerange",
            label = NULL,
            choices = choices,
            selected = "Kernel Density Estimate",
            inline = TRUE
        )
    })
    homerange <- reactive({
        ifelse(input$flt_homerange == "Location Data", FALSE, TRUE)
        })
    
    #___ sel_dailyloc ####
    output$sel_dailyloc <- renderUI({
        req(input$flt_homerange == "Location Data")
        choices <- c("All Locations","Daily Locations")
        radioButtons(
            "flt_dailyloc",
            label = NULL,
            choices = choices,
            selected = "All Locations",
            inline = TRUE
        )
    })
    
    #__ sel_locdate ####
    output$sel_locdate <- renderUI({
        req(input$flt_dailyloc == "Daily Locations")
        
        first_collar_date <- lubridate::as_date("2019-12-14")
        if(input$flt_Date[1] < lubridate::as_date("2019-12-14")){
            selected_date <- first_collar_date
        } else {
            selected_date <- input$flt_Date[1]
        }
        
        sliderInput("flt_locdate",
                    NULL,
                    min = input$flt_Date[1],
                    max = input$flt_Date[2],
                    value = lubridate::as_date("2019-12-14"))
    })
    locdate <- reactive({lubridate::as_date(input$flt_locdate)})

    #___ sel_map_response ####
    output$sel_map_response <- renderUI({
        choices <- c("Total Pictures","Total Number of Animals","Maximum Group Size in Picture")
        choice_names <- as.list(paste0("<b style='font-size:16px'>",choices,"</b>"))
        choice_names <- purrr::map(choice_names,function(.){HTML(.)})
        radioButtons(
            "flt_map_response",
            label = NULL,
            choiceNames = choice_names,
            choiceValues = as.list(choices),
            selected = "Total Pictures",
            inline = TRUE
        )
    })
    
    #___ sel_spec_animal ####
    output$sel_spec_animal <- renderUI({
        req(input$flt_homerange)
        cats <- c("200019","200020","200027","200020","201382","200023","200024","200026","200004","200016","200029")
        choices <- mtlion_index$label[which(mtlion_index$`Animal ID` %in% cats)]
        if(input$flt_homerange == "Kernel Density Estimate"){
            selected <- "200027, Male, SubAdult"
        } else {
            if(input$flt_dailyloc == "Daily Locations"){
                selected <- "All"
            } else {
                selected <- "200027, Male, SubAdult"
            }
        }
        selectInput(
            "flt_spec_animal",
            label = "Select Animal ID",
            choices = c("All",choices),
            selected = selected,
            multiple = TRUE
        )
    })
    spec_animal <- reactive({
        ifelse(input$flt_spec_animal == "All", NA, mtlion_index$`Animal ID`[which(mtlion_index$label %in% input$flt_spec_animal)])
    })

    #___ sel_camID ####
    output$sel_camID <- renderUI({
        choices <- results()$flt_sf_mtlion_pic_grid$CamID
        median_pics <- median(results()$flt_sf_mtlion_pic_grid$numpics, na.rm=T)
        selected <- results()$flt_sf_mtlion_pic_grid$CamID[results()$flt_sf_mtlion_pic_grid$numpics == median_pics]
        selectInput(
            "flt_camID",
            label = "Choose Camera",
            choices = choices,
            selected = selected,
            multiple = TRUE
        )
    })
    
    pic_camera <- reactive({
        req(results())
        df <- results()$flt_sf_mtlion_pic %>%
            dplyr::filter(CamID %in% input$flt_camID)
        obj_filepath <- purrr::map(df$File, function(x){
            if(grepl("Wildlife",getwd())){
                server <- c("K:","Wildlife","Shiny_Apps","EAR","ALL_mountain_lions_R5WINTERSET")
            } else {
                server <- c("/media","kdrive","Wildlife","Shiny_Apps","EAR","ALL_mountain_lions_R5WINTERSET")  
            }
            file <- as.character(x)
            return(paste(c(server, file),collapse="/"))
        })
        return(obj_filepath)
    })
    
    output$slickr_camera <- slickR::renderSlickR({
        
        req(results())
        
        validate(
            need(nrow(results()$flt_sf_mtlion_pic)>0, "No Data Available.")
        )
        
        slideshow <- slickR::slickR(obj = pic_camera(), height = "400px") +
            slickR::settings(centerMode = T,
                             dots = T,
                             centerPadding = '20px',
                             slidesToShow = 1,
                             focusOnSelect = T,
                             lazyLoad = "progressive")
        # print(start)
        # print(end)
        return(slideshow)
    })

    output$mtlion_summary <- renderUI({
        req(results()$flt_sf_mtlion_pic)
        HTML(paste(p(h5("Number of Pictures:",length(unique(results()$flt_sf_mtlion_pic$File)))),
                   p(h5("Number of Days: ",length(unique(results()$flt_sf_mtlion_pic$Date)))),
                   p(h5("Number of Cameras: ",length(unique(results()$flt_sf_mtlion_pic$CamID))))))
    })
    
    output$marked_mtlion_summary <- renderUI({
        req(results()$flt_sf_mtlion_marked_pic)
        HTML(paste(p(h5("Number of Pictures:",length(unique(results()$flt_sf_mtlion_marked_pic$File)))),
                   p(h5("Number of Days: ",length(unique(results()$flt_sf_mtlion_marked_pic$Date)))),
                   p(h5("Number of Cameras: ",length(unique(results()$flt_sf_mtlion_marked_pic$CamID))))))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
