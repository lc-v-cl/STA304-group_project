library(shiny)
library(ggplot2)
library(dplyr)

# Reproduce the original EDA subsampling, preserving the input row order.
pool <- read.csv('survey.csv', stringsAsFactors = FALSE, check.names = FALSE)
set.seed(30412)
dat <- bind_rows(filter(pool, gender == 'Female'),
                 slice_sample(filter(pool, gender == 'Male'), n = 25))
stopifnot(nrow(dat) == 50, sum(dat$gender == 'Female') == 25,
          sum(dat$gender == 'Male') == 25)
# Fail visibly rather than silently displaying a different sample.
stopifnot(abs(mean(dat$sleep_hours_per_night) - 7.28) < .006,
          abs(mean(dat$study_hours_per_week) - 12.81) < .006)

labels <- c(sleep_hours_per_night='Nightly sleep (hours)',
            study_hours_per_week='Weekly study (hours)',
            early_class_days='Early class days per week',
            exercise_days_per_week='Exercise days per week',
            courses_this_semester='Courses this semester',
            sleep_quality='Sleep quality (1–7)')
catlabels <- c(gender='Gender', living_arrangement='Living arrangement', lecture_section='Lecture section')
choices <- setNames(names(labels), unname(labels))
groups <- setNames(names(catlabels), unname(catlabels))
palettes <- list('Bluish Green' = c('#559B96','#A8D5C2','#559B96'),
                 'U of T Blue' = c('#1E3765','#6FC7EA','#1E3765'),
                 'Warm Minimal' = c('#B77B60','#D8B7A6','#8C9D91'))
fmt <- function(x, digits=2) formatC(x, digits=digits, format='f')
pfmt <- function(x) if(!is.finite(x)) 'N/A' else if(x < .001) '< 0.001' else fmt(x,3)
# A conventional pooled-SD Cohen's d, with female minus male direction.
cohen_d <- function(a,b) {
  pooled <- sqrt(((length(a)-1)*var(a)+(length(b)-1)*var(b))/(length(a)+length(b)-2))
  if(!is.finite(pooled) || pooled==0) return(NA_real_)
  (mean(a)-mean(b))/pooled
}

ui <- fluidPage(
  tags$head(tags$meta(name='viewport', content='width=device-width, initial-scale=1'),
    tags$style(HTML('body{background:#F2F5FA;color:#1E3765;font-family:system-ui,Arial,sans-serif} .container-fluid{max-width:1440px;padding:24px} .hero{background:#1E3765;color:white;padding:28px 32px;border-radius:14px;margin-bottom:24px} .hero h1{font-size:32px;font-weight:700;margin:8px 0} .hero p{color:#E2EAF6;margin-bottom:0} .eyebrow{font-size:12px;letter-spacing:2px;font-weight:700;color:#6FC7EA} .well{background:white;border:1px solid #D5DCE8;border-radius:12px;box-shadow:none} .panelcard{background:white;border:1px solid #D5DCE8;border-radius:12px;padding:20px;margin:16px 0} .metric{font-size:30px;font-weight:700;color:#1E3765} .metriclabel{font-size:13px;color:#52637D} .nav-tabs>li>a{color:#1E3765;font-weight:600} .nav-tabs>li.active>a{color:#1E3765} h3{font-size:21px;font-weight:650} .note{color:#52637D;line-height:1.65} .explain{border-left:4px solid #1E3765;padding:12px 16px;background:#EAF0F8;margin:12px 0;line-height:1.65} .shiny-output-error-validation{color:#72551b} .table{font-size:14px;width:100%!important;margin-bottom:12px}.table th{background:#EAF0F8;color:#1E3765}.table td,.table th{padding:10px!important}.panelcard h3{margin-top:4px}.panelcard{overflow-x:auto}.control-label{font-weight:650}.irs-grid-text{font-size:11px}.nav-tabs{margin-bottom:12px}.metric{overflow-wrap:anywhere} .irs-bar,.irs-bar-edge,.irs-from,.irs-to,.irs-single{background:#1E3765!important;border-color:#1E3765!important}.irs-from:after,.irs-to:after,.irs-single:after{border-top-color:#1E3765!important}.btn-default{color:#1E3765;border-color:#AAB9D0;background:#fff}.btn-default:hover,.btn-default:focus{background:#EAF0F8;color:#1E3765;border-color:#1E3765}.nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover{border-top:3px solid #1E3765;color:#1E3765}.form-control:focus,.selectize-input.focus{border-color:#1E3765;box-shadow:0 0 0 2px #D9E3F3}.selectize-dropdown .active{background:#EAF0F8;color:#1E3765}input[type=checkbox]{accent-color:#1E3765}@media(max-width:700px){.container-fluid{padding:12px}.hero{padding:20px}.hero h1{font-size:25px}}'))),
  div(class='hero',div(class='eyebrow','STA304H5F  /  GROUP 12'),
      h1('Student Sleep Explorer'),p('Explore sleep duration, study habits and student characteristics in our gender-balanced survey sample. Use the filters to focus on a subgroup, then choose a research question.')),
  sidebarLayout(
    sidebarPanel(width=3,
      h3('Filter the sample'),
      selectInput('gender','Gender',c('All','Female','Male')),
      selectInput('living','Living arrangement',c('All',sort(unique(dat$living_arrangement)))),
      selectInput('lecture','Lecture section',c('All',sort(unique(dat$lecture_section)))),
      actionButton('reset','Reset filters'),
      tags$hr(),h3('Appearance'),
      selectInput('palette','Colour palette',names(palettes), selected='Bluish Green'),
      p(class='note','Plot-specific axis controls are available on each analysis tab. Axis limits zoom the view without changing the statistical calculations.'),
      textOutput('filter_status')),
    mainPanel(width=9,
      tabsetPanel(id='view',
        tabPanel('Overview',
          div(class='panelcard',fluidRow(column(4,div(class='metriclabel','Students selected'),div(class='metric',textOutput('n'))),column(4,div(class='metriclabel','Mean nightly sleep'),div(class='metric',textOutput('sleepmean'))),column(4,div(class='metriclabel','Mean weekly study'),div(class='metric',textOutput('studymean'))))),
          div(class='panelcard',h3('Explore a distribution'),
              fluidRow(column(6,selectInput('dist','Variable',choices)),column(6,sliderInput('bins','Histogram bins',3,20,8))),
              uiOutput('dist_axis'),plotOutput('distribution',height='340px'),div(class='explain',textOutput('dist_text'))),
          div(class='panelcard',h3('Summary statistics'),tableOutput('summary'),p(class='note','Mean is the average; median is the middle value. SD (standard deviation) describes spread around the mean. IQR (interquartile range) is the width of the middle 50% of responses. N is the number of students. A larger SD or IQR generally indicates more variation.'))),
        tabPanel('Sleep & habits',
          div(class='panelcard',h3('RQ1 · How are study habits related to sleep?'),
              fluidRow(column(6,selectInput('x','Horizontal variable',choices,selected='study_hours_per_week')),column(6,selectInput('y','Vertical variable',choices,selected='sleep_hours_per_night'))),
              checkboxInput('trend','Show fitted linear trend',TRUE),uiOutput('scatter_axes'),
              plotOutput('scatter',height='390px'),div(class='explain',textOutput('scatter_text'))),
          div(class='panelcard',h3('Adjusted model: nightly sleep'),
              p('The original EDA selected study hours, exercise days and living arrangement using AIC. This tab refits that fixed formula for the selected subgroup; it does not repeat variable selection.'),
              tableOutput('regression'),p(class='note','Estimate is the expected change in nightly sleep hours for one additional unit of a predictor, holding the other predictors fixed. The 95% confidence interval (CI) describes uncertainty in that estimate. A p-value below 0.05 is conventionally treated as evidence against a zero coefficient; it is not the probability that the hypothesis is true.'),div(class='explain',textOutput('reg_text')))),
        tabPanel('Group comparison',
          div(class='panelcard',h3('Compare students across groups'),
              fluidRow(column(6,selectInput('group','Group by',groups)),column(6,selectInput('response','Measure',choices))),
              checkboxInput('points','Show individual observations',FALSE),uiOutput('box_axis'),
              plotOutput('boxplot',height='390px'),div(class='explain',textOutput('box_text')),tableOutput('group_summary')),
          div(class='panelcard',h3('RQ2 · Gender comparison: nightly sleep'),p('Welch two-sample t-test compares average nightly sleep between female and male students in the current selection. This test always addresses gender and nightly sleep, regardless of the plot variables above.'),tableOutput('ttest'),div(class='explain',textOutput('ttest_text')))),
        tabPanel('About',div(class='panelcard',h3('Research questions'),p('RQ1: How is weekly study time associated with nightly sleep duration, including after accounting for exercise and living arrangement?'),p('RQ2: Does average nightly sleep duration differ between female and male respondents?'),h3('Survey and sample'),
          p('We collected 84 voluntary responses from STA304H5F students. All 25 female responses and a random sample of 25 of the 54 male responses were retained, using seed 30412. The five responses in the Other and Prefer not to disclose categories were excluded from the balanced sample.'),
          p('The original response pool was voluntary. Balancing gender does not remove selection bias or make this sample representative of the whole course. Exclusion of other gender categories limits what can be concluded.'),
          h3('Measurement'),p('Sleep and study time are self-reported. Study time excludes lectures and tutorials. Early classes start before 10 a.m. Exercise days involve at least 30 minutes of exercise. Sleep quality is an ordinal self-rating from 1 (Terrible) to 7 (Excellent). The cleaned survey recoded study ranges to their midpoints and “Over 25” to 26 hours.'),
          h3('How to interpret results'),p('All associations are exploratory and do not establish causation. Small subgroups yield uncertain estimates. The original model was selected using AIC; reported p-values do not adjust for model selection. A non-significant result does not establish that two groups are equal. Sleep quality ratings are ordinal, so numeric summaries and fitted lines should be treated as exploratory.'),
          h3('Data privacy'),p('Student codenames are removed from the app data. The website does not display a respondent-level data table, but scatterplots and optional individual points show observations. Public sharing of de-identified row-level data requires appropriate permission.'),
          h3('Project team'),p('Junlin Zhu, Qingyu Li, Tejas Kumar, Jing Li')))
      )
    )
  ),
  tags$footer(class='note','STA304H5F · Group 12 · Exploratory survey analysis')
)

server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateSelectInput(session,'gender',selected='All'); updateSelectInput(session,'living',selected='All'); updateSelectInput(session,'lecture',selected='All')
  })
  filtered <- reactive({
    d <- dat
    if(input$gender != 'All') d <- d[d$gender == input$gender,]
    if(input$living != 'All') d <- d[d$living_arrangement == input$living,]
    if(input$lecture != 'All') d <- d[d$lecture_section == input$lecture,]
    d
  })
  available <- reactive({d <- filtered(); validate(need(nrow(d)>0,'No respondents match these filters. Try a broader selection.')); d})
  cols <- reactive(palettes[[input$palette]])
  style <- function() theme_minimal(base_size=15) + theme(panel.grid.minor=element_blank(), plot.title=element_text(face='bold',color='#1E3765'), axis.title=element_text(color='#344B70',size=13),axis.text=element_text(size=12,color='#344B70'), plot.title.position='plot', legend.position='bottom')
  output$filter_status <- renderText(paste(nrow(filtered()),'of 50 respondents selected'))
  output$n <- renderText(nrow(filtered()))
  output$sleepmean <- renderText(if(nrow(filtered())==0) 'N/A' else paste0(fmt(mean(filtered()$sleep_hours_per_night)), ' h'))
  output$studymean <- renderText(if(nrow(filtered())==0) 'N/A' else paste0(fmt(mean(filtered()$study_hours_per_week)), ' h'))
  axis_slider <- function(id, var, title) {
    r <- range(dat[[var]],na.rm=TRUE); if(r[1]==r[2]) r <- r+c(-1,1)
    if(id=='distlim') {
      pad <- diff(r)/4; r <- c(max(0,floor(r[1]-pad)),ceiling(r[2]+pad))
    } else { r <- c(floor(r[1]),ceiling(r[2])) }
    sliderInput(id,title,min=r[1],max=r[2],value=r,step=if(var=='sleep_hours_per_night' && id!='distlim') .5 else 1, ticks=FALSE)
  }
  output$dist_axis <- renderUI(axis_slider('distlim',input$dist,'Horizontal axis limits'))
  output$scatter_axes <- renderUI(fluidRow(column(6,axis_slider('xlim',input$x,'Horizontal axis limits')),column(6,axis_slider('ylim',input$y,'Vertical axis limits'))))
  output$box_axis <- renderUI(axis_slider('boxlim',input$response,'Vertical axis limits'))
  output$distribution <- renderPlot({d<-available(); v<-input$dist; req(input$distlim); ggplot(d,aes(x=.data[[v]]))+geom_histogram(aes(fill=after_stat(factor((match(x,sort(unique(x)))-1L) %% 2L))),bins=input$bins,colour='white')+scale_fill_manual(values=c('0'=cols()[1],'1'=cols()[2]),guide='none')+coord_cartesian(xlim=input$distlim)+labs(title=paste('Distribution of',labels[[v]]),x=labels[[v]],y='Number of students')+style()})
  output$dist_text <- renderText({x<-available()[[input$dist]]; paste0('Among the ',length(x),' selected students, the average is ',fmt(mean(x)),', the median is ',fmt(median(x)),', and responses range from ',fmt(min(x)),' to ',fmt(max(x)),'. The bars show how many students fall into each interval. The spread of the middle half of responses (IQR) is ',fmt(IQR(x)),'. ',if(mean(x)>median(x)) 'The mean is above the median, which can reflect larger values in the upper tail.' else if(mean(x)<median(x)) 'The mean is below the median, which can reflect smaller values in the lower tail.' else 'The mean and median coincide, although this does not guarantee a symmetric distribution.',if(input$dist=='sleep_quality') ' This is an ordinal rating; the average is descriptive only.' else '')})
  output$summary <- renderTable({d<-available(); data.frame(Measure=unname(labels),Mean=sapply(d[names(labels)],mean),Median=sapply(d[names(labels)],median),SD=sapply(d[names(labels)],sd),IQR=sapply(d[names(labels)],IQR),N=nrow(d),row.names=NULL)},digits=2,striped=TRUE)
  output$scatter <- renderPlot({d<-available(); req(input$xlim,input$ylim); x<-input$x;y<-input$y; p<-ggplot(d,aes(x=.data[[x]],y=.data[[y]]))+geom_point(colour=cols()[1],size=2.8,alpha=.7); if(input$trend && nrow(d)>=3 && length(unique(d[[x]]))>1 && x!=y) p<-p+geom_smooth(method='lm',formula=y~x,colour=cols()[2],se=TRUE); p+coord_cartesian(xlim=input$xlim,ylim=input$ylim)+labs(title=paste(labels[[y]],'vs',labels[[x]]),x=labels[[x]],y=labels[[y]])+style()})
  output$scatter_text <- renderText({d<-available();x<-d[[input$x]];y<-d[[input$y]]; if(input$x==input$y) return('Choose different variables to explore an association.'); if(length(x)<3||sd(x)==0||sd(y)==0) return('Not enough variation or observations to estimate an association.'); r<-cor(x,y); b<-coef(lm(y~x))[2]; strength<-if(abs(r)<.3) 'weak' else if(abs(r)<.7) 'moderate' else 'strong'; paste0('In the selected sample (n = ',length(x),'), the correlation is ',fmt(r),'. ',if(abs(r)<.005) 'There is almost no linear association.' else paste0('This suggests a ',strength,' ',if(r<0) 'negative' else 'positive',' linear association: higher values of the horizontal variable tend to accompany ',if(r<0) 'lower' else 'higher',' values of the vertical variable.'),' A one-unit increase in ',labels[[input$x]],' is associated with a fitted change of ',fmt(b),' units in ',labels[[input$y]],'. ',if(input$trend) 'The shaded band is a 95% confidence interval for the fitted mean response, not a prediction interval for individual students. ' else '', 'This is an unadjusted association, not evidence of causation.')})
  reg <- reactive({d<-available(); validate(need(nrow(d)>=10,'Select at least 10 respondents for the adjusted model.'),need(length(unique(d$living_arrangement))>=2,'Select at least two living arrangements to fit the original model.')); d$living_arrangement<-factor(d$living_arrangement,levels=sort(unique(d$living_arrangement))); m<-lm(sleep_hours_per_night~study_hours_per_week+exercise_days_per_week+living_arrangement,data=d); validate(need(m$rank==length(coef(m)) && df.residual(m)>0,'Insufficient variation to estimate the model reliably.'),need(all(is.finite(coef(summary(m)))) && is.finite(summary(m)$r.squared) && summary(m)$sigma>0,'The selected responses do not support reliable model uncertainty estimates.')); m})
  output$regression <- renderTable({m<-reg(); a<-coef(summary(m)); ci<-confint(m); nm<-c('(Intercept)'=paste0('Intercept (',m$xlevels$living_arrangement[1],')'),study_hours_per_week='Weekly study hours',exercise_days_per_week='Weekly exercise days'); terms<-rownames(a); readable<-ifelse(terms %in% names(nm),unname(nm[terms]),sub('living_arrangement','Living: ',terms,fixed=TRUE)); data.frame(Predictor=readable,Estimate=fmt(a[,1],3),`95% CI lower`=fmt(ci[,1],3),`95% CI upper`=fmt(ci[,2],3),`p-value`=sapply(a[,4],pfmt),check.names=FALSE)},striped=TRUE)
  output$reg_text <- renderText({m<-reg(); s<-summary(m); b<-coef(m)['study_hours_per_week']; p<-s$coefficients['study_hours_per_week',4]; paste0('The model accounts for ',fmt(100*s$r.squared,1),'% of the observed variation in nightly sleep. Holding exercise days and living arrangement fixed, an additional weekly study hour is associated with ',fmt(abs(b)*60,1),' minutes ',if(b<0) 'less' else 'more',' sleep per night (p ',if(p<.001) '< 0.001' else paste0('= ',pfmt(p)),'). ',if(p<.05) 'The association meets the conventional 5% significance threshold.' else 'The evidence is insufficient at the conventional 5% significance threshold.',paste0(' The reference living arrangement is ',m$xlevels$living_arrangement[1],'. '),paste(vapply(setdiff(names(coef(m)),c('(Intercept)','study_hours_per_week')),function(term){v<-coef(m)[term]; pv<-s$coefficients[term,4]; what<-if(term=='exercise_days_per_week') 'One additional exercise day per week' else paste0(sub('living_arrangement','',term,fixed=TRUE),' compared with ',m$xlevels$living_arrangement[1]); paste0(what,' is associated with ',fmt(abs(v)*60,1),' minutes ',if(v<0) 'less' else 'more',' nightly sleep, holding other predictors fixed (p ',if(!is.finite(pv)) '= N/A' else if(pv<.001) '< 0.001' else paste0('= ',pfmt(pv)),'). ',if(is.finite(pv)&&pv<.05) 'The association meets the 5% threshold.' else 'The evidence does not establish an association at the 5% threshold.')},character(1)),collapse=' '),' This is exploratory, not causal; p-values do not account for AIC selection.')})
  output$boxplot <- renderPlot({d<-available();req(input$boxlim);g<-input$group;v<-input$response;p<-ggplot(d,aes(x=.data[[g]],y=.data[[v]]))+geom_boxplot(fill=cols()[1],alpha=.8,outlier.shape=if(input$points) NA else 19); if(input$points) p<-p+geom_point(position=position_jitter(width=.12,height=0,seed=12),alpha=.5,size=1.8); p+scale_x_discrete(labels=function(x) vapply(x,function(z) paste(strwrap(z,width=20),collapse='\n'),character(1)))+coord_cartesian(ylim=input$boxlim)+labs(title=paste(labels[[v]],'by',catlabels[[g]]),x=catlabels[[g]],y=labels[[v]],fill=catlabels[[g]])+style()+theme(axis.text.x=element_text(angle=0,hjust=.5),legend.position='none')})
  grouped <- reactive({d<-available();d |> group_by(Group=.data[[input$group]]) |> summarise(N=n(),Mean=mean(.data[[input$response]]),Median=median(.data[[input$response]]),IQR=IQR(.data[[input$response]]),.groups='drop')})
  output$group_summary <- renderTable(grouped(),digits=2,striped=TRUE)
  output$box_text <- renderText({g<-grouped(); paste0('The line inside each box is the median, and the box covers the middle half of the responses. Whiskers extend to the most extreme observations within 1.5 IQR of the box; points beyond them are potential outliers, not necessarily errors. ',paste(paste0(g$Group,': median ',fmt(g$Median),' (n = ',g$N,')'),collapse='; '),'. ',if(nrow(g)<2) 'Only one group is selected, so no between-group comparison is possible.' else paste0(if(length(unique(g$IQR))>1) paste0('The largest middle-half spread is in ',paste(g$Group[g$IQR==max(g$IQR)],collapse=' and '),'; the smallest is in ',paste(g$Group[g$IQR==min(g$IQR)],collapse=' and '),'. ') else 'The groups have the same middle-half spread. ','Differences visible in the boxes are descriptive and do not by themselves establish statistical significance.'))})
  tt <- reactive({d<-available(); a<-d$sleep_hours_per_night[d$gender=='Female']; b<-d$sleep_hours_per_night[d$gender=='Male']; validate(need(length(a)>=2 && length(b)>=2,'At least two female and two male respondents are needed for the test.')); t<-tryCatch(t.test(a,b),error=function(e) NULL); validate(need(!is.null(t),'The selected responses do not support a Welch t-test.')); t})
  effect <- reactive({d<-available(); cohen_d(d$sleep_hours_per_night[d$gender=='Female'],d$sleep_hours_per_night[d$gender=='Male'])})
  output$ttest <- renderTable({t<-tt(); data.frame(`Female mean`=fmt(t$estimate[1]),`Male mean`=fmt(t$estimate[2]),`Difference (F − M)`=fmt(diff(rev(t$estimate))),`95% CI`=paste0('[',fmt(t$conf.int[1]),', ',fmt(t$conf.int[2]),']'),`p-value`=pfmt(t$p.value),`Cohen's d`=if(is.finite(effect())) fmt(effect()) else 'N/A',check.names=FALSE)},striped=TRUE)
  output$ttest_text <- renderText({t<-tt(); paste0('The estimated female minus male difference is ',fmt(t$estimate[1]-t$estimate[2]),' hours (',fmt(abs(t$estimate[1]-t$estimate[2])*60,1),' minutes in absolute terms). ',if(t$p.value<.05) 'The selected sample provides evidence of a difference in mean sleep duration at the 5% level.' else 'There is insufficient evidence of a difference in mean sleep duration at the 5% level; this does not prove the means are equal.',' The 95% confidence interval gives a range of plausible differences under the test assumptions. ',if(is.finite(effect())) paste0("Cohen's d = ",fmt(effect()),' expresses the mean difference in pooled standard deviation units. ',if(abs(effect())<.2) 'The observed standardized difference is very small.' else if(abs(effect())<.5) 'The observed standardized difference is small.' else if(abs(effect())<.8) 'The observed standardized difference is moderate.' else 'The observed standardized difference is large.',' These size labels are rough guidelines, not evidence of significance.') else 'The standardized difference cannot be estimated reliably.')})
}
shinyApp(ui,server)
