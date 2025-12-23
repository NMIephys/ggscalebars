#' Title
#'
#' @param start start of the bar
#' @param end end of the bar
#' @param line line where the bar appears
#' @param style a list of style parameters
#' @param filter_expr condition when the bar should be drawn; mostly used in conjunction with facetting. 
#' @param label labe of the topbar
#' @param line_to.x,line_to.x2 x position of 1-2 lines from bar downwards, defaults to start and end
#' @param line_to.y,line_to.y2 where ot draw a line: numeric (y coordinates), I() for absolute, or "data" 
#' @param line_to.color,line_to.size,line_to.linetype style of the lines  
#' @param line_to.arrow optional, define an arrow at end of line using arrow=arrow(...)
#' @param line_to.gap when lines_to,y="data", place a small gap between line and data
#' @param label.x,label.y completely override label positioning, use I() for absolute coordinates 
#' @param label.position on of "center", "above", "below", "left", "right"
#' @param fill color of the bar
#' @param border optional border color of the bar
#' @param label.col color  of label
#' @param label.size size of label
#' @param get_data function to modify plot data before calculating the topspace. 
#' @param hjust,vjust override automatic hjust and vjust for the labels

#' @param ... unused
#'
#' @export
geom_topbar<-function(
             start,
             end,
             line = 1,
             #fixed.y = NA,
             #sweeps = "all",
             #label.sweeps = sweeps,
             label = "",
             
             line_to.x=start,
             line_to.x2=end,
             line_to.y=label.y,
             line_to.y2=label.y,
             line_to.color=fill,
             line_to.size=1,
             line_to.linetype=1,
             line_to.arrow=arrow(length=unit(0,"mm")),
             line_to.gap=0,
             #bar.mapping=NA,
             label.x =label.xpos(label.position, start, end),
             label.y =label.ypos(label.position, line, style),
             label.position=c("center", "above", "below", "left", "right"),
             fill = "grey",
             border = {{fill}},
             label.col = "black",
             label.size=5,
             get_data=unfiltered,
             hjust = label.hjust(label.position),
             vjust = label.vjust(label.position),
             style=ggsweeps.defaultstyle,
             filter_expr=TRUE,
             
             ...) {#start, end, line=1, label.x=start + (end - start)/2, style=ggsweeps.defaultstyle,filter_expr=TRUE){
  
  
  list(
    #bar
    geom_rect(xmin=start, xmax=end, na.rm=T,
                ymin=I(1-style$height*line-style$space*(line-1)-style$topspace),
                ymax=I(1-style$height*(line-1)-style$space*(line-1)-style$topspace),
                data=. %>% get_data %>% filter({{filter_expr}}) %>%
                  head(1), # prevents overplotting multiple times
                fill=fill, linewidth=1,
                color=border
                ),
    #label
    geom_text(x=label.x,
               y=label.y, na.rm=T,
               label=label, color=label.col, size=label.size, vjust=vjust,hjust=hjust, show.legend=F,
               data=. %>% get_data %>% filter({{filter_expr}}) %>%
                 head(1) # prevents overplotting multiple times
               #data=NULL
               ),
    
    # # line.to
    if(! line_to.y=="data"){
    geom_segment(x=start, xend=line_to.x, y=label.y, yend=line_to.y, color=line_to.color, linewidth=line_to.size, arrow=line_to.arrow, linetype=line_to.linetype,na.rm=T,
                  data=. %>% get_data %>% filter({{filter_expr}}) %>%
                    head(1))
    }else{
    
    # line.to data
    geom_segment(aes(yend=y + line_to.gap),x=start, xend=line_to.x, y=label.y, color=line_to.color, linewidth=line_to.size, arrow=line_to.arrow, linetype=line_to.linetype,na.rm=T,
                 data=. %>% get_data %>% filter({{filter_expr}}) %>% filter(x==line_to.x) %>%
                   head(1))
    },
    
    # line2.to
    if(! line_to.y2=="data"){
      geom_segment(x=end, xend=line_to.x2, y=label.y, yend=line_to.y2, color=line_to.color, linewidth=line_to.size, arrow=line_to.arrow, linetype=line_to.linetype,na.rm=T,
                    data=. %>% get_data %>% filter({{filter_expr}}) %>%
                      head(1))
    }else{
      print("linesto2 to data")
      geom_segment(aes(yend=y + line_to.gap), x=end, xend=line_to.x2, y=label.y, color=line_to.color, linewidth=line_to.size, arrow=line_to.arrow, linetype=line_to.linetype,na.rm=T,
                   data=. %>% get_data %>% filter({{filter_expr}}) %>% filter(x==line_to.x2) %>%
                     head(1))
    }
    ,
    
    # reserve space
    geom_blank(
      data=. %>% get_data %>% filter({{filter_expr}}) %>% mutate(...x=start), 
      stat="summary", 
      fun=fun_topspace(
           line   *style$height +
          (line+1)*style$space+
           style$bottomspace+
            
           style$topspace), na.rm=T,
      aes(x=...x) # this solves a problem: what if x is mapped to something else?
    ) 
  )
}


label.hjust<-function(position=c("center", "above", "below", "left", "right")){
  position=match.arg(position)
  #print(position)
  switch(position, center=0.5, above = 0.5, below=0.5, left=1, right=0)
}
label.vjust<-function(position=c("center", "above", "below", "left", "right")){
  position=match.arg(position)
  switch(position, center=0.4, above = -.2, below=1, left=0.4, right=0.4)
}

label.xpos<-function(position=c("center", "above", "below", "left", "right"), start, end){
  position=match.arg(position)
  switch(position, center=start+(end-start)/2, above = start+(end-start)/2, below=start+(end-start)/2, left=start, right=end)
}

label.ypos<-function(position=c("center", "above", "below", "left", "right"), line, style){
  position=match.arg(position)
  y=1-style$height*(line-1)-style$height/2-style$space*(line-1)-style$topspace
  y=switch(position, center=y, left=y, right=y, above=y+style$height/2, below=y-style$height/2)
  #print(position)
  I(y)
}

fun_topspace<-function( space_for_bars = .1){
  function(y){
    y=y[!is.na(y)]
    #print(theme_get()$legend.key.size)
    max(y, na.rm=T) + diff(range(y)) * (space_for_bars/(1-space_for_bars))    
    
  }
}


