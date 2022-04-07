#Creado por Arturo Sirvent Fresneda  
# Hosteado en shiny io :   https://artsir.shinyapps.io/covidviz/

#packages = c("ggmap","leaflet","tmap","geospatial","sp","ggthemes","lubridate","tmaptools","plotly","rsconnect","shiny","shinythemes")

# #use this function to check if each package is on the local machine
# #if a package is installed, it will be loaded
# #if any are not, the missing package(s) will be installed and loaded
# package.check <- lapply(packages, FUN = function(x) {
#   if (!require(x, character.only = TRUE)) {
#     install.packages(x, dependencies = TRUE,repos='http://cran.rediris.es')
#     library(x, character.only = TRUE)
#   }
# })

library(rsconnect)
library(shiny)
library(lubridate)
library(shinythemes)
library(plotly)
library(tidyverse)
library(maps)
library(geospatial)
library(sp)
library(ggmap)
library(ggthemes)
library(tmap)
library(tmaptools)
library(shiny)
library(shinythemes)





#antes de nada, descargamos y preprocesamos los datos

confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
death <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
df.confirmed <- read_csv(confirmed)
df.death <- read_csv(death)
df.recovered <- read_csv(recovered)



df.confirmed.aux<-subset(df.confirmed,select=-c(`Province/State`,Lat,Long))
df.confirmed.delta<-subset(df.confirmed,select=c(`Country/Region`))

for (i in 3:length(df.confirmed.aux)){
  dia_aux<-names(df.confirmed.aux)[i]
  dia_aux_prev<-names(df.confirmed.aux)[i-1]
  #calculamos la diferencia entre ambos dias
  diff_aux<-df.confirmed.aux[dia_aux]-df.confirmed.aux[dia_aux_prev]
  df.confirmed.delta[dia_aux]<-diff_aux
}
#para muertos
df.death.aux<-subset(df.death,select=-c(`Province/State`,Lat,Long))
df.death.delta<-subset(df.death,select=c(`Country/Region`))

for (i in 3:length(df.death.aux)){
  dia_aux<-names(df.death.aux)[i]
  dia_aux_prev<-names(df.death.aux)[i-1]
  #calculamos la diferencia entre ambos dias
  diff_aux<-df.death.aux[dia_aux]-df.death.aux[dia_aux_prev]
  df.death.delta[dia_aux]<-diff_aux
}

#para recuperados
df.recovered.aux<-subset(df.recovered,select=-c(`Province/State`,Lat,Long))
df.recovered.delta<-subset(df.recovered,select=c(`Country/Region`))

for (i in 3:length(df.recovered.aux)){
  dia_aux<-names(df.recovered.aux)[i]
  dia_aux_prev<-names(df.recovered.aux)[i-1]
  #calculamos la diferencia entre ambos dias
  diff_aux<-df.recovered.aux[dia_aux]-df.recovered.aux[dia_aux_prev]
  df.recovered.delta[dia_aux]<-diff_aux
}



df.confirmed.delta[df.confirmed.delta<0]<-0
df.death.delta[df.death.delta<0]<-0
df.recovered.delta[df.recovered.delta<0]<-0


conf_grouped<-df.confirmed.aux %>% group_by(`Country/Region`) %>%summarise(across(everything(),sum))
conf_delta_grouped<-df.confirmed.delta %>% group_by(`Country/Region`) %>%summarise(across(everything(),sum))

#death
death_grouped<-df.death.aux %>% group_by(`Country/Region`) %>%summarise(across(everything(),sum))
death_delta_grouped<-df.death.delta %>% group_by(`Country/Region`) %>%summarise(across(everything(),sum))

#recovered
rec_grouped<-df.recovered.aux %>% group_by(`Country/Region`) %>%summarise(across(everything(),sum))
rec_delta_grouped<-df.recovered.delta %>% group_by(`Country/Region`) %>%summarise(across(everything(),sum))

data(World)
W<-World

W$name<-as.vector(W$name)

W$name[W$name=="Korea"]<-"Korea, South"
W$name[W$name=="United States"]<-"US"
W$name[W$name=="Taiwan*"]<-"Taiwan."
W$name[W$name=="S. Sudan"]<-"South Sudan"
W$name[W$name=="Central African Rep."]<-"Central African Republic"
W$name[W$name=="Congo"]<-"Congo (Brazzaville)"
W$name[W$name=="Macedonia"]<-"North Macedonia"
W$name[W$name=="Bosnia and Herz."]<-"Bosnia and Herzegovina"


#ahora añadimos las columnas a ese dataframe
#primero para la incidencia acumulada
conf_complete<-merge(W,conf_grouped,by.x="name",by.y="Country/Region")
death_complete<-merge(W,death_grouped,by.x="name",by.y="Country/Region")
rec_complete<-merge(W,rec_grouped,by.x="name",by.y="Country/Region")

#ahora para la diaria
conf_delta_complete<-merge(W,conf_delta_grouped,by.x="name",by.y="Country/Region")
death_delta_complete<-merge(W,death_delta_grouped,by.x="name",by.y="Country/Region")
rec_delta_complete<-merge(W,rec_delta_grouped,by.x="name",by.y="Country/Region")


master_df <- conf_delta_grouped %>% pivot_longer(cols = -c(1) ,names_to = "fecha",values_to = "confirmed_delta") 
#tenemos que convertir en fechas la columna fecha
master_df<-master_df %>% mutate(fecha=mdy(fecha))%>%arrange(fecha,`Country/Region`)

df_aux<- death_delta_grouped %>% pivot_longer(cols = -c(1) ,names_to = "fecha",values_to = "death_delta") 
df_aux<-df_aux %>% mutate(fecha=mdy(fecha))%>%arrange(fecha,`Country/Region`)

master_df["death_delta"]<-df_aux["death_delta"]

df_aux<- rec_delta_grouped %>% pivot_longer(cols = -c(1) ,names_to = "fecha",values_to = "recovered_delta") 
df_aux<-df_aux %>% mutate(fecha=mdy(fecha))%>%arrange(fecha,`Country/Region`)

master_df["recovered_delta"]<-df_aux["recovered_delta"]


#y ahora le añadimos los lat long 
master_df<-merge(master_df,df.confirmed %>% select(c(`Country/Region`,Lat,Long)),by="Country/Region")
str(master_df)


master_df<-master_df %>% pivot_longer(cols=c(confirmed_delta,death_delta,recovered_delta),names_to = "type")


# codigos <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")
# 
# #aqui pasa lo mismo que con el dataset World
# master_df$`Country/Region`[master_df$`Country/Region`=="Korea"]<-"Korea, South"
# master_df$`Country/Region`[master_df$`Country/Region`=="United States"]<-"US"
# master_df$`Country/Region`[master_df$`Country/Region`=="Taiwan*"]<-"Taiwan."
# master_df$`Country/Region`[master_df$`Country/Region`=="S. Sudan"]<-"South Sudan"
# master_df$`Country/Region`[master_df$`Country/Region`=="Central African Rep."]<-"Central African Republic"
# master_df$`Country/Region`[master_df$`Country/Region`=="Congo"]<-"Congo (Brazzaville)"
# master_df$`Country/Region`[master_df$`Country/Region`=="Macedonia"]<-"North Macedonia"
# master_df$`Country/Region`[master_df$`Country/Region`=="Bosnia and Herz."]<-"Bosnia and Herzegovina"
# 
# conf_codigos<-merge(master_df,codigos%>%select(c("COUNTRY","CODE")),by.x="Country/Region",by.y="COUNTRY")





ui<-navbarPage("Visualización covid creada por Arturo Sirvent Fresneda para el Master Ciencia de Datos (UV), 2022",theme=shinytheme("simplex"),
               sidebarLayout(
                 sidebarPanel(
                   h5("Opciones"),
                   selectInput("dia","Selecciona el día (Year-Month-Day)",choices = master_df%>%arrange(fecha)%>%select(fecha)%>%unique()),
                   selectInput("grupo","Selecciona el grupo",choices =c("Confirmed"="confirmed_delta","Death"="death_delta","Recovered"="recovered_delta")),
                   selectInput("lista_paises1","Selecciona el país 1 para la grafica de lineas",choices=c("Null",master_df%>%arrange(`Country/Region`)%>%select(`Country/Region`)%>%unique()),
                               selected=master_df$`Country/Region`[1]),
                   selectInput("lista_paises2","Selecciona el país 2 para la grafica de lineas",choices=c("Null",master_df%>%arrange(`Country/Region`)%>%select(`Country/Region`)%>%unique()),
                               selected=master_df$`Country/Region`[1]),
                   dataTableOutput("data")
                 ),
                 #el plot este en plotly da problemas
                 mainPanel(
                   h5("Mapa"),
                   plotlyOutput("plot1"),
                   plotlyOutput("plot2"),
                   h5("El mapa cloropleth data error al cargarlo en leafLet, diculpen las molestias, en su lugar le obsequiamos con un gato."),
                   br(),
                   img(src='cat.jpeg', align = "center")
                   )
                 )
               )

server <- function(input, output) {
  #updateSelectInput(session,"dia",choices = )
  #updateSelectInput(session,"grupo",choices = alarmas())
  aux_table<-reactive({master_df%>%filter(fecha==input$dia,type==input$grupo)})
  lista_paises<-reactive({c(input$lista_paises1,input$lista_paises2)})
  #dia_aux<-reactive({format(input$dia,"%m/%d/%y")})
  # world<-reactive({
  #   ggplot(data = df.confirmed[df.confirmed[input$dia]!=0,]) +
  #   borders("world", colour = "gray85", fill = "gray80") +
  #   theme_map() })
  # 
  # map <- reactive({world + geom_point(aes_string(x = "Long", y = "Lat", size= paste0("`",input$dia,"`") ), 
  #                                     colour = 'red', alpha = .5)+scale_size_area(max_size=10)})
  output$plot1<-renderPlotly({ggplot(data = aux_table()) +
      borders("world", colour = "gray85", fill = "gray80") +
      theme_map() + geom_point(aes(x = Long, y = Lat, size= value), colour = 'red', alpha = .5)+scale_size_area(max_size=10)})
    
  output$data<-renderDataTable(aux_table()%>%select(`Country/Region`,value)%>%group_by(`Country/Region`)%>%summarise(valor=sum(value)))
  

  output$plot2<-renderPlotly({ggplot(data = master_df %>% filter( master_df$`Country/Region` %in% lista_paises() ), 
                                     aes(x=fecha,y=value)) + geom_line(aes(color=`Country/Region`))+ facet_wrap(~type,scales = "free",nrow=3)})#,nrow=length(lista_paises())
  
  
  #output$map_output<-renderPlotly({conf_codigos %>% filter(type==input$grupo)%>%plot_geo() %>% add_trace(type='choropleth', locations=~CODE,z=~value,frame=input$day)})
  
  #output$map_output<-renderPlotly({
  #  c<-tm_shape(rec_delta_complete) +
  #    tm_polygons(input$dia,n=10,style="pretty") + tm_layout(title="Recovered")})
  # b<-tm_shape(death_delta_complete) +
  #   tm_polygons(dia_aux(),n=10,style="pretty")+ tm_layout(title="Deaths")
  # 
  # 
  # c<-tm_shape(rec_delta_complete) +
  #   tm_polygons(dia_aux(),n=10,style="pretty") + tm_layout(title="Recovered")
  # 
  # tmap_arrange(a,b,c)})

}

shinyApp(ui, server)

  