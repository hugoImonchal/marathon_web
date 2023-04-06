
function(input, output) {


  
  tableData = reactiveValues(
    w_var=NULL,
    myData=NULL,
    p_var=NULL,
    CorVF=NULL
    )
  
  #Create a proxy for updating table
  proxy = dataTableProxy('myTable')
  proxy2 = dataTableProxy('poids_var')
  
  #Render table
  output$myTable = renderDT({
    
    if (!(is.null(tableData$myData))){
      isolate(tableData$myData)
    }
    
  }, editable = "cell")
  
  
  
  #Render table
  output$poids_var = renderDT({

    if (!(is.null(tableData$myData))){
      isolate(tableData$w_var)
    }
    
  }, editable = "cell", options = list(searching = FALSE, lengthChange = FALSE,
                                       info=FALSE, paging=FALSE))
  
  
  #Detect cell edits
  observeEvent(input$myTable_cell_edit, {
    
    cell = input$myTable_cell_edit

    if(cell$col == 1 & as.numeric(cell$value)>=0){
      
      tableData$myData[cell$row,cell$col] = as.numeric(cell$value)

      replaceData(proxy, tableData$myData)
      
    }
    
  })
  
  
  
  #Detect cell edits
  observeEvent(input$poids_var_cell_edit, {
    
    cell = input$poids_var_cell_edit
    
  
    if (as.integer(cell$value)>=0 & sum(tableData$w_var)>=3){
      tableData$w_var[cell$row,cell$col] = as.integer(cell$value)
      replaceData(proxy2, tableData$w_var)
    }
    
    if(sum(tableData$w_var)>=2 & as.integer(cell$value)>0){
      tableData$w_var[cell$row,cell$col] = as.integer(cell$value)
      replaceData(proxy2, tableData$w_var)
    }
    
    # print(tableData$w_var)
    
    
  })
  
  
  
  observeEvent(input$action, {
    
    ExtracWPCA<-WPCA(tableData$myData, tableData$w_var)
    tableData$myData<-ExtracWPCA$X
    tableData$p_var=ExtracWPCA$per_var
    tableData$CorVF=ExtracWPCA$corr
    replaceData(proxy, tableData$myData)
    
    if(ncol(tableData$CorVF)==2){
      updateRadioButtons(inputId="axes", label = "Axes", choices = c("1-2"="1-2"),
                         selected = "1-2")
    }else{
      updateRadioButtons(inputId="axes", label = "Axes", choices = c("1-2"="1-2", "1-3"="1-3", "2-3"="2-3"),
                         selected = "1-2")
    }
    
  })
  
  
  
  observeEvent(input$submit_research, {
    if(input$words=="Bibliometric"){
      file<-"https://www.bibliometrix.org/datasets/savedrecs.bib"
      # file<-"bibliometrics.bib"
      tableData$myData<-create(file)
      
      tableData$w_var<-data.frame(Annee=c(1), Nombre_references=c(1), Nombre_citations=c(1),
                                  Utilisation_last_180j=c(1),h_index_auteur=c(1),
                                  h_index_source=c(1), Facteur_dominance_auteur=c(1))
      
      ExtracWPCA<-WPCA(tableData$myData, tableData$w_var)
      tableData$myData<-ExtracWPCA$X
      tableData$p_var=ExtracWPCA$per_var
      tableData$CorVF=ExtracWPCA$corr
      
      replaceData(proxy, tableData$myData)
      replaceData(proxy2, tableData$w_var)
      
      output$refresh_wpca <- renderUI({
                actionButton(inputId = "action", "Réeffectuer une ACP")
              })
      
      output$choix_axes <- renderUI({
        radioButtons("axes", "Axes", c("1-2"="1-2", "1-3"="1-3", "2-3"="2-3"), selected="1-2")
      })
      
      
      output$poids_variables_texte <- renderText({
        "Vous pouvez modifier l'importance des variables dans le tableau ci dessous, afin que l'ACP vous propose des résultats satisfaisants :"
      })
      
      output$results_texte <- renderText({
        "Voici les résultats.
        Vous pouvez modifier la note des articles proposés afin d'améliorer le système de recommandation."
      })
      
      
      output$controle_annee <- renderUI({
        
        mini<-min(tableData$myData$Annee)
        maxi<-max(tableData$myData$Annee)
        
        sliderInput(inputId = "range", "Range", min = mini, max = maxi,
                    value=c(mini,maxi))
      })
      
      
      
      
    }else{
      tableData$myData<-NULL
      tableData$w_var<-NULL
      replaceData(proxy, tableData$myData)
      replaceData(proxy2, tableData$w_var)
      
      tableData$p_var<-NULL
      tableData$CorVF<-NULL
      
      output$refresh_wpca <- renderUI({})
      output$choix_axes <- renderUI({})
      
      output$poids_variables_texte <- renderText({})
      output$results_texte <- renderText({})
      
      output$controle_annee <- renderUI({})
      
    }
    
  })
  
  
  
  output$pres_evol <- renderText({
    
    if(!(is.null(tableData$myData))){
    
      mini<-input$range[1]
      maxi<-input$range[2]
      
      t<-as.data.frame(table(tableData$myData$Annee))
      colnames(t)<-c("Year", "Articles")
      t$Year<-as.numeric(as.character(t$Year))
      t<- t[t["Year"]>=mini & t["Year"]<=maxi, ]
      
      nbr_min<-t[t["Year"]==mini, "Articles"]
      nbr_max<-t[t["Year"]==maxi, "Articles"]
      
      tx_crois<-(((nbr_max/nbr_min)^(1/(maxi-mini)))-1)*100
      
      paste(sum(t$Articles), " articles ont été récolés sur la période allant de",
            mini, " à ", maxi, "avec un taux moyen de croissance de publication de ", 
            round(tx_crois,2), "%.")
    }
  })
  
  output$plot_evol_nbr_art_tps <- renderPlot({
    
    if(!(is.null(tableData$myData))){
      
      mini<-input$range[1]
      maxi<-input$range[2]
      t<-as.data.frame(table(tableData$myData$Annee))
      colnames(t)<-c("Year", "Articles")
      t$Year<-as.numeric(as.character(t$Year))
      t<- t[t["Year"]>=mini & t["Year"]<=maxi, ]
      p <- ggplot(data = t, aes(x = Year, y = Articles, group = 1))
      p + geom_line(color="blue") + geom_point() + 
        labs(title = paste("Nombre d'articles publiés entre ", mini, " et ", maxi), x = "Années", y = "Nombre d'articles")
    }
  })
  
  output$pres_pays <- renderText({
    if (is.null(tableData$myData)){
      tableData$myData
    }else {
      mini<-input$range[1]
      maxi<-input$range[2]
      tmp <- tableData$myData[tableData$myData["Annee"]>=mini & tableData$myData["Annee"]<=maxi, ]
      c<-as.data.frame(table(tmp$Pays_Auteur_Principal))
      paste("Dans cette base de données, on dénombre ", nrow(c),
            " pays, avec un nombre moyen de publication de ", round(mean(c$Freq), 2), ".")
    }
  })
  
  
  output$plot_prod_pays <- renderPlot({
    if (is.null(tableData$myData)){
      tableData$myData
    }else {
      mini<-input$range[1]
      maxi<-input$range[2]
      
      tmp <- tableData$myData[tableData$myData["Annee"]>=mini & tableData$myData["Annee"]<=maxi, ]
      
      c<-as.data.frame(table(tmp$Pays_Auteur_Principal))
      c<-c[order(c$Freq, decreasing=TRUE),]
      if (nrow(c)>10){
        c<-head(c, 10)
      }
      ggplot(data=c, aes(x = reorder(Var1, -Freq), y = Freq))+
        geom_bar(stat="identity", width=0.5)+theme(axis.text.x=element_text(angle=60, hjust=1))+
        labs(y = "Nombre de publications", x = "Pays", title = paste("Nombre de publications par pays entre ", mini, " et ", maxi))
      
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$pres_source <- renderText({
    if (is.null(tableData$myData)){
      tableData$myData
    }else {
      mini<-input$range[1]
      maxi<-input$range[2]
      tmp <- tableData$myData[tableData$myData["Annee"]>=mini & tableData$myData["Annee"]<=maxi, ]
      c<-as.data.frame(table(tmp$Journal))
      paste("Dans cette base de données, on dénombre ", nrow(c),
            " journaux, avec un nombre moyen de publication de ", round(mean(c$Freq), 2), ".")
    }
  })
  
  
  output$plot_prod_source <- renderPlot({
    if (is.null(tableData$myData)){
      tableData$myData
    }else {
      mini<-input$range[1]
      maxi<-input$range[2]
      
      tmp <- tableData$myData[tableData$myData["Annee"]>=mini & tableData$myData["Annee"]<=maxi, ]
      
      c<-as.data.frame(table(tmp$Journal))
      c<-c[order(c$Freq, decreasing=TRUE),]
      if (nrow(c)>10){
        c<-head(c, 10)
      }
      ggplot(data=c, aes(x = reorder(Var1, -Freq), y = Freq))+
        geom_bar(stat="identity", width=0.5)+
        labs(y = "Nombre de publications", x = "Journal", title = paste("Nombre de publications par journaux entre ", mini, " et ", maxi))+ coord_flip()
      
    }
  })
  
  
  
  output$pres_authors <- renderText({
    if(!(is.null(tableData$myData))){
      mini<-input$range[1]
      maxi<-input$range[2]
      tmp<- tableData$myData[tableData$myData["Annee"]>=mini & tableData$myData["Annee"]<=maxi, ]
      dfauth <- as.data.frame(table(tmp$Principal_auteur))
      colnames(dfauth)<-c("Auteur_principal", "Nombre_publication")
      paste(nrow(dfauth), " auteurs principaux sont ressourcés entre ",
            mini, " et ", maxi, "avec un taux de publication moyen de ", 
            round(mean(dfauth$Nombre_publication),2), ".")
    }
  })
  
  
  output$plot_authors <- renderPlot({
    if(!(is.null(tableData$myData))){
      mini<-input$range[1]
      maxi<-input$range[2]
      tmp<- tableData$myData[tableData$myData["Annee"]>=mini & tableData$myData["Annee"]<=maxi, ]
      dfauth <- as.data.frame(table(tmp$Principal_auteur))
      colnames(dfauth)<-c("Auteur_principal", "Nombre_publication")
      dfauth<-dfauth[order(dfauth$Nombre_publication, decreasing = TRUE),]
      if (nrow(dfauth)>10){
        dfauth<-head(dfauth, 10)
      }
      ggplot(data = dfauth, aes(x = reorder(Auteur_principal, -Nombre_publication), y = Nombre_publication)) + 
        geom_bar(stat = "identity")+
        labs(y = "Nombre de publications", x = "Auteurs", title = paste("Nombre de publications par auteurs entre ", mini, " et ", maxi))
    }
  })
  
  observeEvent(input$axes, {
    output$plot_scatter_WPCA <- renderPlotly({
      if(!(is.null(tableData$myData))){
        if(input$axes=="1-2"){
          g<-ggplot(data=tableData$myData, aes(label=Titre))+geom_point(aes(x=CoordF1, y=CoordF2))
          ggplotly(g)
        }else{
        if(input$axes=="1-3"){
          g<-ggplot(data=tableData$myData, aes(label=Titre))+geom_point(aes(x=CoordF1, y=CoordF3))
          ggplotly(g)
        }else{
        if(input$axes=="2-3"){
          g<-ggplot(data=tableData$myData, aes(label=Titre))+geom_point(aes(x=CoordF2, y=CoordF3))
          ggplotly(g)
        }}}
      }
    })
    
    output$plot_cor_WPCA <- renderPlot({
      if(!(is.null(tableData$myData))){
        if(input$axes=="1-2"){
          
          ggplot(data=tableData$CorVF)+coord_fixed(ratio = 1)+
            geom_segment(aes(x = 0, y = 0, xend = CoordF1, yend = CoordF2), size = 1.2, colour="grey",
                         arrow = arrow(length = unit(0.5, "cm")))+
            geom_text(data = tableData$CorVF, aes(x=CoordF1, y=CoordF2, label = rownames(tableData$CorVF)), size=5)+
            annotate("path", x=cos(seq(0,2*pi,length.out=100)),
                      y=sin(seq(0,2*pi,length.out=100)))+
            labs(x = "CoordF1", y = "CoordF2", title = "Cercle de corrélation" )

        }else{
          if(input$axes=="1-3"){
            ggplot(data=tableData$CorVF)+coord_fixed(ratio = 1)+
              geom_segment(aes(x = 0, y = 0, xend = CoordF1, yend = CoordF3), size = 1.2, colour="grey",
                           arrow = arrow(length = unit(0.5, "cm")))+
              geom_text(data = tableData$CorVF, aes(x=CoordF1, y=CoordF3, label = rownames(tableData$CorVF)), size=5)+
              annotate("path", x=cos(seq(0,2*pi,length.out=100)),
                        y=sin(seq(0,2*pi,length.out=100)))+
              labs(x = "CoordF1", y = "CoordF3", title = "Cercle de corrélation" )
            
          }else{
            if(input$axes=="2-3"){
              ggplot(data=tableData$CorVF)+coord_fixed(ratio = 1)+
                geom_segment(aes(x = 0, y = 0, xend = CoordF2, yend = CoordF3), size = 1.2, colour="grey",
                             arrow = arrow(length = unit(0.5, "cm")))+
                geom_text(data = tableData$CorVF, aes(x=CoordF2, y=CoordF3, label = rownames(tableData$CorVF)), size=5)+
                annotate("path", x=cos(seq(0,2*pi,length.out=100)),
                          y=sin(seq(0,2*pi,length.out=100)))+
                labs(x = "CoordF2", y = "CoordF3", title = "Cercle de corrélation" )
            }}}
      }
    })
    
  
  })
    
  
  

  
  
}




