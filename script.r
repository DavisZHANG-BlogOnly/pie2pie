source('./r_files/flatten_HTML.r')

############### Library Declarations ###############
libraryRequireInstall("ggplot2");

libraryRequireInstall("plotly");
libraryRequireInstall("dplyr");
################### Actual code ####################

ThemeColor = "black"
if(exists("myCustomVisualsObj_lineColor")){
  ThemeColor = myCustomVisualsObj_lineColor
}

labelsColor = "black"
if(exists("myCustomVisualsObj_labelsColor")){
  labelsColor = myCustomVisualsObj_labelsColor
}

labelsPosition = "auto"
if(exists("myCustomVisualsObj_labelsPosition")){
  labelsPosition = myCustomVisualsObj_labelsPosition
}

labels = "none"
if(exists("myCustomVisualsObj_setDataLabels")){
  labels = myCustomVisualsObj_setDataLabels
}

showLine = FALSE
if(exists("myCustomVisualsObj_showLine")){
  showLine = myCustomVisualsObj_showLine
}

showLegend = FALSE
if(exists("myCustomVisualsObj_showLegend")){
  showLegend = myCustomVisualsObj_showLegend
}

Legendborder = FALSE
if(exists("myCustomVisualsObj_showLegendborder")){
  Legendborder = myCustomVisualsObj_showLegendborder
}

borderwidth = 0
if(Legendborder){
  borderwidth = 1
}

mergeSmallerValues = FALSE
if(exists("myCustomVisualsObj_mergeSmallerValues")){
  mergeSmallerValues = myCustomVisualsObj_mergeSmallerValues
}

LegendPosition = "top"
if(exists("myCustomVisualsObj_LegendPosition")){
  LegendPosition = myCustomVisualsObj_LegendPosition
}

legendTextsize = 10
if(exists("myCustomVisualsObj_legendTextsize")){
  legendTextsize = myCustomVisualsObj_legendTextsize
}

if (LegendPosition == "top"){
  lx = 0.5
  ly= 1.2
  lo = "h"  
} else if (LegendPosition == "bottom"){
  lx = 0.5
  ly= 0
  lo = "h"  
}

direction = "default"
if(exists("sizeConfig_direction")){
  direction = sizeConfig_direction
}

radius_1 = 0
if(exists("sizeConfig_size_a")){
  radius_1 = sizeConfig_size_a
}

radius_2 = 0
if(exists("sizeConfig_size_b")){
  radius_2 = sizeConfig_size_b
}

showPlotlyPane = FALSE
if(exists("otherConfig_plotlyBar")){
  showPlotlyPane = otherConfig_plotlyBar
}

####################################################

validToPlot=TRUE
if(!exists("C1")||!exists("V1")&&validToPlot==TRUE)
{ 
  validToPlot = FALSE
  t <- list(
  family = "sans serif",
  size = 20,
  color = toRGB("grey50"))

  Warning = "Category or Values may missed !"
  pp <- plot_ly(x = 1,y = 1,text = Warning) %>%
  add_text(textfont = t, textposition = "center") %>%
  layout(showlegend = F,
       xaxis = list(visible=FALSE),
       yaxis = list(visible=FALSE))
  
} else if(
  length(as.character(unlist(C1))) != length(as.character(unlist(V1)))
  ){
  validToPlot = FALSE
  t <- list(
  family = "sans serif",
  size = 15,
  color = toRGB("grey50"))

  Warning = "Make sure that Category and Values have the same length."
  pp <- plot_ly(x = 1,y = 1,text = Warning) %>%
  add_text(textfont = t, textposition = "center") %>%
  layout(showlegend = F,
       xaxis = list(visible=FALSE),
       yaxis = list(visible=FALSE))  
} else {
  C1 <- as.character(unlist(C1));
  V1 <- as.numeric(unlist(V1));  
  data <- data.frame(C1,V1,stringsAsFactors=FALSE)
  data <- aggregate(data$V1,by = list(C1 = data$C1),FUN = sum,na.rm = TRUE)
  names(data)<-c("C1","V1")
  rownames(data) <- data$C1
}

if(validToPlot==TRUE&&exists("data")){
if(as.numeric(sum(t(data[,2])))==0){
  validToPlot = FALSE
  t <- list(
  family = "sans serif",
  size = 20,
  color = toRGB("grey50"))

  Warning = " Total value cannot be Zero."
  pp <- plot_ly(x = 1,y = 1,text = Warning) %>%
  add_text(textfont = t, textposition = "center") %>%
  layout(showlegend = F,
       xaxis = list(visible=FALSE),
       yaxis = list(visible=FALSE)) 
  } else {  
  min <- function(x,n) {
    value = sort(x, FALSE)[n]
    column_name = colnames(x)[which(x == value, arr.ind = TRUE)[2]]
    paste0(value)
  }
  
  min_value <- as.numeric(min(t(data[,2]),1))
  n <- 1
  while(min_value/as.numeric(sum(t(data[,2]))) < 0.1) {
    n = n+1
    min_value = min_value+as.numeric(min(t(data[,2]),n))
  }
  
  data <- data[order(-data$V1,data$C1),]
  sub_data = tail(data,n)

  rotation = min_value/as.numeric(sum(t(data[,2])))*180+90
  
  deg2rad <- function(deg) {(deg * pi) / (180)}
  x=c(sin(deg2rad(180-rotation))*2 + 4,8)
  y1 = c(sin(deg2rad(rotation-90))*2 + 5.5,6.5)
  y2 = c(-sin(deg2rad(rotation-90))*2 + 4.5,3.5)

  if(mergeSmallerValues){
    data_2 <- head(data,-n)
    data_3 <- data.frame("Others",sum(sub_data$V1))
    names(data_3)<-c("C1","V1")
    rownames(data_3) <- data_3$C1
    data <- rbind(data_2,data_3)
    nn <- 0
    for(i in 1:tail(rank(data$V1,ties.method = "min"),1))
      nn = nn + as.numeric(min(t(data[,2]),i))
      nr = as.numeric(data[which(data$C1 == "Others"),]["V1"])/2
          
    rotation = (nn-nr)/as.numeric(sum(t(data[,2])))*360+90
  }

  if(direction == "default"){
    x_m = c(0.0, 0.6)
    y_m = c(0, 1)
    x_s = c(0.7, 1.0)
    y_s = c(0.1, 0.9)
  } else if(direction == "reverse"){
    x_s = c(0.0, 0.3)
    y_s = c(0.1, 0.9)
    x_m = c(0.4, 1.0)
    y_m = c(0, 1)

    x[2] = 7-(4-sin(deg2rad(rotation-90))*2)
    x[1] = 1.5
    y1[1] = y1[2]
    y1[2] = sin(deg2rad(rotation-90))*2 + 4.8
    y2[1] = y2[2]
    y2[2] = -sin(deg2rad(rotation-90))*2 + 5.2
    rotation = rotation - 180
  }

  if(showLegend){
    gap = -0.15
    y1 = y1 - gap
    y2 = y2 + gap
  }

  pp <- plot_ly(type = "pie",textinfo=labels,textposition = labelsPosition,
    textfont = list(color = labelsColor),
    sort = TRUE) %>%
    add_pie(data = data, labels = ~C1, values = ~V1,showlegend = showLegend,
            name = "Main Pie", direction = "clockwise",hole = radius_1,
            rotation = rotation,
            marker = list(line = list(color = ThemeColor,width = 1)),
            domain = list(x = x_m, y = y_m)) %>%
    add_pie(data = sub_data, labels = ~C1, values = ~V1,hole = radius_2,
            showlegend = showLegend,name = "Sub Pie",   
            marker = list(line = list(color = ThemeColor,width = 1)), 
            domain = list(x = x_s, y = y_s)) %>%
    layout(legend=list(x=lx,y=ly,orientation = lo),
           xaxis = list(visible=FALSE,showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(visible=FALSE,showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
    layout(legend = list(bordercolor = ThemeColor,borderwidth = borderwidth,
      font = list(color = labelsColor,size = legendTextsize),itemsizing="trace")) %>%
    layout(paper_bgcolor='transparent',plot_bgcolor='transparent',
      xaxis = list(range=c(0,10)),
      yaxis = list(range=c(0,10))) %>%
    layout(xaxis=list(fixedrange=TRUE),yaxis=list(fixedrange=TRUE))
  
  if(showLine){
    pp <- pp %>% 
      add_lines(x = ~x,y = ~y1, name = "linear1", hoverinfo = "none",showlegend = FALSE,
        line = list(color = ThemeColor,shape = "linear",dash='dash',width = 1))%>%
      add_lines(x = ~x,y = ~y2, name = "linear2", hoverinfo = "none",showlegend = FALSE,
        line = list(color = ThemeColor,shape = "linear",dash='dash',width = 1))
  }

  if(showPlotlyPane){
    pp <- pp %>% config(displayModeBar = T)
  } else {
    pp <- pp %>% config(displayModeBar = F)
  }

}
}
####################################################

############# Create and save widget ###############
p = ggplotly(pp);
internalSaveWidget(p, 'out.html');
####################################################
