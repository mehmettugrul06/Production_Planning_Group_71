# Keep wide tables inside their own horizontal scroll container.
ap_scroll_table <- function(id) {
  shiny::div(
    class = 'ap-scroll-table',
    style = 'width:100%; max-width:100%; overflow-x:auto; overflow-y:hidden; margin-bottom:14px; border:1px solid #e5e7eb; border-radius:5px;',
    shiny::tableOutput(id)
  )
}

# Shiny module: Aggregate Planning. Requires R/aggregate_core.R to be sourced.
aggregate_planning_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::tags$style(shiny::HTML('.ap-scroll-table table { white-space: nowrap; width: max-content; min-width: 100%; } .ap-scroll-table { min-width: 0; }')) ,
    shiny::h3('Aggregate Planning'),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width=4,
        shiny::fileInput(ns('datafile'),'Upload demand data (.xlsx or .csv)',accept=c('.xlsx','.csv')),
        shiny::helpText('Required columns: Period, Demand, Days. Sample data is used until a file is uploaded.'),
        shiny::uiOutput(ns('columns_ui')),
        shiny::textInput(ns('period_names'),'Edit period names (comma-separated)',''),
        shiny::tags$hr(),
        shiny::radioButtons(ns('k_mode'),'Productivity K (units/worker/day)',
          c('Direct entry'='direct','Calculate from past data'='historical'),inline=FALSE),
        shiny::conditionalPanel(sprintf("input['%s'] == 'direct'",ns('k_mode')),
          shiny::numericInput(ns('k'),'K',2,min=.00001)),
        shiny::conditionalPanel(sprintf("input['%s'] == 'historical'",ns('k_mode')),
          shiny::numericInput(ns('past_units'),'Historical units produced',500,min=.00001),
          shiny::numericInput(ns('past_workers'),'Historical average workers',10,min=.00001),
          shiny::numericInput(ns('past_days'),'Historical working days',25,min=.00001)),
        shiny::uiOutput(ns('k_view')),
        shiny::numericInput(ns('w0'),'Starting workforce',10,min=0),
        shiny::numericInput(ns('i0'),'Starting inventory',0,min=0),
        shiny::numericInput(ns('ie'),'Required ending inventory',0,min=0),
        shiny::tags$h4('Costs (per worker or unit)'),
        shiny::numericInput(ns('hire'),'Hiring cost (per worker)',200,min=0),
        shiny::numericInput(ns('fire'),'Firing cost (per worker)',300,min=0),
        shiny::numericInput(ns('hold'),'Inventory holding cost (unit/period)',2,min=0),
        shiny::numericInput(ns('regular'),'Regular production cost (unit)',10,min=0),
        shiny::numericInput(ns('overtime'),'Overtime cost (unit)',15,min=0),
        shiny::numericInput(ns('sub'),'Subcontracting cost (unit)',20,min=0),
        shiny::numericInput(ns('idle'),'Idle-capacity cost (unit of capacity)',0,min=0),
        shiny::numericInput(ns('ot_frac'),'Overtime max as fraction of regular capacity',.25,min=0,max=5,step=.05),
        shiny::numericInput(ns('sub_cap'),'Subcontracting capacity per period',100000,min=0),
        shiny::actionButton(ns('run'),'Calculate plans',class='btn-primary')
      ),
      shiny::mainPanel(width=8,
        shiny::tabsetPanel(id=ns('method'),
          shiny::tabPanel('Chase',value='Chase',shiny::uiOutput(ns('chase_results'))),
          shiny::tabPanel('Level',value='Level',shiny::uiOutput(ns('level_results'))),
          shiny::tabPanel('Mixed',value='Mixed',
            shiny::uiOutput(ns('mixed_choices')),shiny::uiOutput(ns('mixed_results'))),
          shiny::tabPanel('Linear Programming',value='LP',
            shiny::checkboxInput(ns('allow_ot'),'Allow overtime',TRUE),
            shiny::checkboxInput(ns('allow_idle'),'Allow idle time',TRUE),
            shiny::checkboxInput(ns('allow_sub'),'Allow subcontracting',TRUE),
            shiny::uiOutput(ns('lp_results'))),
          shiny::tabPanel('Transportation',value='Transportation',
            shiny::selectInput(ns('transport_workforce'),'Workforce source',
              choices=c('Level plan'='Level','Chase plan'='Chase','Mixed plan'='Mixed','LP plan'='LP')),
            shiny::radioButtons(ns('transport_round'),'Workforce scenario',
              choices=c('Fractional workers'='fractional','Whole workers (rounded up)'='whole'),inline=TRUE),
            shiny::uiOutput(ns('transport_results'))),
          shiny::tabPanel('Disaggregation',value='Disaggregation',
            shiny::selectInput(ns('source_plan'),'Aggregate source plan',
              choices=c('Chase','Level','Mixed','LP')),
            shiny::helpText('Square-root shares: sqrt(weight) / sum(sqrt(weight)). Enter family weights below.'),
            shiny::textAreaInput(ns('family_text'),'Product families: one Family,Weight per line',
              'Family A,64\nFamily B,25\nFamily C,9',rows=5),
            shiny::uiOutput(ns('disagg_results')))
        )
      )
    )
  )
}
aggregate_planning_server <- function(id) {
 shiny::moduleServer(id,function(input,output,session) {
  sample_data <- data.frame(Period=paste0('Month ',1:6),
    Demand=c(195,210,180,235,260,220),Days=c(20,22,20,21,22,20))
  uploaded <- shiny::reactive({
    if(is.null(input$datafile)) return(sample_data)
    f<-input$datafile$datapath; ext<-tolower(tools::file_ext(input$datafile$name))
    if(ext=='csv') {
      x<-utils::read.csv(f,check.names=FALSE,stringsAsFactors=FALSE)
    } else if(ext=='xlsx') {
      if(!requireNamespace('readxl',quietly=TRUE)) stop('Install package readxl for Excel files.')
      x<-as.data.frame(readxl::read_excel(f))
    } else stop('Only .xlsx and .csv are supported.')
    if(nrow(x)<1 || ncol(x)<3) stop('File requires at least 3 columns and 1 data row.')
    x
  })
  output$columns_ui <- shiny::renderUI({
    x<-uploaded(); col<-names(x)
    shiny::tagList(
      shiny::selectInput(session$ns('period_col'),'Period column',col,selected=if('Period'%in%col) 'Period' else col[1]),
      shiny::selectInput(session$ns('demand_col'),'Demand column',col,selected=if('Demand'%in%col) 'Demand' else col[min(2,length(col))]),
      shiny::selectInput(session$ns('days_col'),'Working days column',col,selected=if('Days'%in%col) 'Days' else col[min(3,length(col))])
    )
  })
  shiny::observeEvent(input$datafile,shiny::updateTextInput(session,'period_names',value=''))
  data <- shiny::reactive({
    x<-uploaded(); shiny::req(input$period_col,input$demand_col,input$days_col)
    d<-data.frame(Period=as.character(x[[input$period_col]]),
      Demand=suppressWarnings(as.numeric(x[[input$demand_col]])),
      Days=suppressWarnings(as.numeric(x[[input$days_col]])))
    if(nzchar(trimws(input$period_names))) {
      p<-trimws(strsplit(input$period_names,',',fixed=TRUE)[[1]])
      if(length(p)!=nrow(d) || any(!nzchar(p))) stop('Period names must match the number of rows.')
      d$Period<-p
    }
    if(anyNA(d$Demand) || anyNA(d$Days)) stop('Demand and Days must be numeric with no missing values.')
    d
  })
  output$mixed_choices <- shiny::renderUI({
    d<-data()
    shiny::checkboxGroupInput(session$ns('mixed_starts'),
      'Start periods of new workforce blocks (Period 1 is always a start)',
      choices=stats::setNames(as.character(seq_len(nrow(d))[-1]), d$Period[-1]),
      selected=if(nrow(d)>=4) as.character(4) else character())
  })
  K <- shiny::reactive({
    if(identical(input$k_mode,'historical')) {
      shiny::validate(shiny::need(input$past_workers>0 && input$past_days>0,'Historical workers and days must be positive.'))
      input$past_units/(input$past_workers*input$past_days)
    } else input$k
  })
  output$k_view <- shiny::renderUI({shiny::helpText(sprintf('Current K: %.4f units/worker/day',K()))})
  costs <- shiny::reactive({
    list(hire=input$hire,fire=input$fire,hold=input$hold,regular=input$regular,
         overtime=input$overtime,sub=input$sub,idle=input$idle,
         ot_frac=input$ot_frac,sub_cap=input$sub_cap)
  })
  params <- shiny::eventReactive(input$run,{
    d<-data();k<-K();w0<-input$w0;i0<-input$i0;ie<-input$ie;cc<-costs()
    ap_check(d,k,w0,i0,ie)
    if(any(vapply(cc,function(z) !is.finite(z)||z<0,logical(1)))) stop('All cost and capacity inputs must be nonnegative.')
    if(i0>sum(d$Demand)+ie+1e-7) stop('Initial inventory exceeds demand plus ending inventory.')
    list(d=d,k=k,w0=w0,i0=i0,ie=ie,c=cc)
  },ignoreNULL=FALSE)
  execute <- function(fun) tryCatch(fun(),error=function(e) list(error=conditionMessage(e)))
  get_plan <- function(method,round_up=FALSE) {
    x<-params()
    starts<-as.integer(input$mixed_starts)
    if(method %in% c('Chase','Level','Mixed'))
      return(ap_heuristic(method,x$d,x$k,x$w0,x$i0,x$ie,x$c,starts,round_up))
    if(method=='LP') {
      frac<-ap_lp(x$d,x$k,x$w0,x$i0,x$ie,x$c,
        isTRUE(input$allow_ot),isTRUE(input$allow_idle),isTRUE(input$allow_sub))
      if(!round_up) return(frac)
      return(ap_lp(x$d,x$k,x$w0,x$i0,x$ie,x$c,
        isTRUE(input$allow_ot),isTRUE(input$allow_idle),isTRUE(input$allow_sub),
        fixed_w=ceiling(frac$Workers-1e-7)))
    }
    stop('Unknown plan method.')
  }
  results <- shiny::reactive({
    method<-input$method
    # Make tab options reactive; run button freezes common data/cost inputs only.
    if(method %in% c('Chase','Level','Mixed','LP')) {
      return(execute(function() list(frac=get_plan(method,FALSE),whole=get_plan(method,TRUE))))
    }
    NULL
  })
  plot_plan <- function(p,d,i0,main) {
    n<-nrow(p); actual<-cumsum(p$Production)
    required<-cumsum(d$Demand)-i0
    y<-range(c(0,actual,required),finite=TRUE)
    graphics::plot(seq_len(n),actual,type='o',pch=16,col='#2563eb',lwd=2,
      xlab='Period',ylab='Cumulative units',main=main,xaxt='n',ylim=y)
    graphics::axis(1,at=seq_len(n),labels=p$Period,las=2,cex.axis=.75)
    graphics::lines(seq_len(n),required,type='o',pch=17,col='#dc2626',lwd=2)
    graphics::legend('topleft',legend=c('Cumulative production','Cumulative net demand'),
      col=c('#2563eb','#dc2626'),lty=1,pch=c(16,17),bty='n',cex=.8)
  }
  register_pair <- function(tag,method) {
    output[[paste0(tag,'_results')]] <- shiny::renderUI({
      r<-if(identical(input$method,method)) results() else NULL
      if(is.null(r)) return(shiny::p('Select this method and click Calculate plans.'))
      if(!is.null(r$error)) return(shiny::div(class='alert alert-danger',r$error))
      x<-params()
      shiny::tagList(
        shiny::h4(method,'— fractional vs. whole workers'),
        shiny::div(style='width:100%; min-width:0;',
          shiny::h4('Fractional workers'),
          ap_scroll_table(session$ns(paste0(tag,'_frac'))),
          shiny::h5('Cost breakdown — fractional workers'),
          ap_scroll_table(session$ns(paste0(tag,'_frac_cost'))),
          shiny::tags$hr(),
          shiny::h4('Whole workers (rounded up)'),
          ap_scroll_table(session$ns(paste0(tag,'_whole'))),
          shiny::h5('Cost breakdown — whole workers'),
          ap_scroll_table(session$ns(paste0(tag,'_whole_cost')))),
        shiny::plotOutput(session$ns(paste0(tag,'_plot')),height='330px'),
        shiny::downloadButton(session$ns(paste0(tag,'_download')),'Download both plans (CSV)')
      )
    })
    output[[paste0(tag,'_frac')]]<-shiny::renderTable({shiny::req(input$method==method);r<-results();shiny::req(is.null(r$error));r$frac},digits=2)
    output[[paste0(tag,'_whole')]]<-shiny::renderTable({shiny::req(input$method==method);r<-results();shiny::req(is.null(r$error));r$whole},digits=2)
    output[[paste0(tag,'_frac_cost')]]<-shiny::renderTable({r<-results();shiny::req(is.null(r$error),input$method==method);ap_cost(r$frac,params()$c)},digits=2)
    output[[paste0(tag,'_whole_cost')]]<-shiny::renderTable({r<-results();shiny::req(is.null(r$error),input$method==method);ap_cost(r$whole,params()$c)},digits=2)
    output[[paste0(tag,'_plot')]]<-shiny::renderPlot({r<-results();shiny::req(is.null(r$error),input$method==method);x<-params();plot_plan(r$frac,x$d,x$i0,method)})
    output[[paste0(tag,'_download')]]<-shiny::downloadHandler(
      filename=function() paste0('aggregate_',tolower(tag),'_plans.csv'),
      content=function(file) {r<-results();shiny::req(is.null(r$error));
        utils::write.csv(rbind(data.frame(Scenario='Fractional',r$frac),
          data.frame(Scenario='Whole workers',r$whole)),file,row.names=FALSE)})
  }
  register_pair('chase','Chase');register_pair('level','Level')
  register_pair('mixed','Mixed');register_pair('lp','LP')
  output$transport_results<-shiny::renderUI({
    shiny::req(input$method=='Transportation')
    r<-transport_results()
    if(!is.null(r$error)) return(shiny::div(class='alert alert-danger',r$error))
    shiny::tagList(
      shiny::p('Greedy methods provide starting allocations. Optimal LP certifies the minimum transportation allocation cost for the selected workforce.'),
      ap_scroll_table(session$ns('transport_summary')),
      shiny::selectInput(session$ns('transport_show'),'Display allocation',
        choices=c('North-West Corner','Cheapest Cell First','Optimal LP')),
      shiny::h4('Period plan'),ap_scroll_table(session$ns('transport_plan')),
      shiny::h4('Shipment allocation'),ap_scroll_table(session$ns('transport_ship')),
      shiny::h4('Cost breakdown'),ap_scroll_table(session$ns('transport_cost')),
      shiny::plotOutput(session$ns('transport_plot'),height='330px')
    )
  })
  transport_results<-shiny::reactive({
    shiny::req(input$method=='Transportation')
    execute(function(){
      x<-params();w<-get_plan(input$transport_workforce,identical(input$transport_round,'whole'))$Workers
      methods<-c('North-West Corner','Cheapest Cell First','Optimal LP')
      outs<-lapply(methods,function(m) tryCatch(ap_transport(x$d,x$k,x$w0,x$i0,x$ie,x$c,w,m),
                      error=function(e) list(error=conditionMessage(e))))
      names(outs)<-methods
      list(results=outs)
    })
  })
  selected_transport<-shiny::reactive({
    r<-transport_results();shiny::req(is.null(r$error),input$transport_show)
    out<-r$results[[input$transport_show]]
    shiny::validate(shiny::need(is.null(out$error),if(!is.null(out$error)) out$error else TRUE))
    out
  })
  output$transport_summary<-shiny::renderTable({
    r<-transport_results();shiny::req(is.null(r$error))
    data.frame(Method=names(r$results),
      `Transportation objective`=vapply(r$results,function(z) if(is.null(z$error)) z$transport_cost else NA_real_,numeric(1)),
      `Total plan cost`=vapply(r$results,function(z) if(is.null(z$error)) tail(ap_cost(z$plan,params()$c)$Cost,1) else NA_real_,numeric(1)),
      Status=vapply(r$results,function(z) if(is.null(z$error)) 'Feasible' else z$error,character(1)),check.names=FALSE)
  })
  output$transport_plan<-shiny::renderTable({selected_transport()$plan},digits=2)
  output$transport_ship<-shiny::renderTable({selected_transport()$shipments},digits=2)
  output$transport_cost<-shiny::renderTable({ap_cost(selected_transport()$plan,params()$c)},digits=2)
  output$transport_plot<-shiny::renderPlot({x<-params();plot_plan(selected_transport()$plan,x$d,x$i0,'Transportation plan')})
  disagg_result <- shiny::reactive({
    shiny::req(input$method=='Disaggregation')
    execute(function(){
      lines<-trimws(strsplit(input$family_text,'\n',fixed=TRUE)[[1]])
      lines<-lines[nzchar(lines)]
      pairs<-strsplit(lines,',',fixed=TRUE)
      if(length(pairs)==0 || any(lengths(pairs)!=2)) stop('Use one Family,Weight entry per line.')
      fam<-data.frame(Family=trimws(vapply(pairs,`[`,character(1),1)),
        Weight=suppressWarnings(as.numeric(trimws(vapply(pairs,`[`,character(1),2)))))
      if(anyDuplicated(fam$Family) || any(!nzchar(fam$Family))) stop('Family names must be unique and nonempty.')
      p<-get_plan(input$source_plan,FALSE)
      p_whole<-get_plan(input$source_plan,TRUE)
      list(data=ap_disaggregate(p,fam),plan=p,
           whole_data=ap_disaggregate(p_whole,fam),whole_plan=p_whole)
    })
  })
  output$disagg_results<-shiny::renderUI({
    r<-disagg_result()
    if(!is.null(r$error)) return(shiny::div(class='alert alert-danger',r$error))
    shiny::tagList(shiny::h4('Product-family plan — fractional workforce'),ap_scroll_table(session$ns('disagg_table')),
      shiny::h4('Product-family plan — whole workforce'),ap_scroll_table(session$ns('disagg_whole_table')),
      shiny::h4('Aggregate plan costs — fractional workforce'),ap_scroll_table(session$ns('disagg_cost')),
      shiny::h4('Aggregate plan costs — whole workforce'),ap_scroll_table(session$ns('disagg_whole_cost')),
      shiny::plotOutput(session$ns('disagg_plot'),height='330px'),
      shiny::downloadButton(session$ns('disagg_download'),'Download disaggregation (CSV)'))
  })
  output$disagg_table<-shiny::renderTable({disagg_result()$data},digits=3)
  output$disagg_cost<-shiny::renderTable({ap_cost(disagg_result()$plan,params()$c)},digits=2)
  output$disagg_whole_table<-shiny::renderTable({disagg_result()$whole_data},digits=3)
  output$disagg_whole_cost<-shiny::renderTable({ap_cost(disagg_result()$whole_plan,params()$c)},digits=2)
  output$disagg_plot<-shiny::renderPlot({x<-params();plot_plan(disagg_result()$plan,x$d,x$i0,'Source aggregate plan')})
  output$disagg_download<-shiny::downloadHandler(
    filename=function() 'disaggregation.csv',
    content=function(file) {r<-disagg_result();utils::write.csv(rbind(
       data.frame(Scenario='Fractional',r$data),data.frame(Scenario='Whole',r$whole_data)),file,row.names=FALSE)})
 })
}
