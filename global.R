#Librairies

library(shiny)
library(bibliometrix)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)

#Create DataFrame 

create <- function(file){
  
  #BibTex to Df
  M <- convert2df(file = file, dbsource = "isi", format = "bibtex")
  
  #Analyse bibliometrique des donnees
  results <- biblioAnalysis(M, sep = ";")
  
  #Extraction du pays des auteurs
  AU_CO <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
  
  #Colonnes que l'on conserve
  cols<-c("AU_CO", "AU", "TI","NR","PY", "SC", "SO", "PU", "DI", "DB", "RP", "U2", "TC")
  var<-AU_CO[,cols]
  
  #Extraction auteur principal
  var["F_AU"]=sapply(var$AU, function(x) strsplit(x, ";")[[1]][1])
  
  #Extraction du pays de l'auteur principal 
  var["F_AU_CO"]=sapply(var$AU_CO, function(x) strsplit(x, ";")[[1]][1])
  
  #Extraction h_index pour l'auteur principal
  authors=var$F_AU
  indices <- Hindex(var, field = "author", elements=authors, sep = ";", years = 50)
  h_index_aut<-indices$H[,c("Element", "h_index")]
  colnames(h_index_aut)<-c("F_AU", "h")
  var<-left_join(var,h_index_aut, by="F_AU")
  
  #Extraction h_index pour la source
  source=var$SO
  indices <- Hindex(var, field = "source", elements=source, sep = ";", years = 50)
  h_index_so<-indices$H[,c("Element", "h_index")]
  colnames(h_index_so)<-c("SO", "h_so")
  var<-left_join(var,h_index_so, by="SO")
  
  
  #Extraction domination de l'auteur principal
  d<-dominance(results, k=length(results$FirstAuthors))
  d<-d[,c("Author", "Dominance Factor")]
  colnames(d)<-c("F_AU", "DOM")
  var<-left_join(var,d, by="F_AU")
  
  #Creation de la colonne Note
  var<-cbind(var, rep(1,nrow(var)))
  colnames(var)[ncol(var)]<-"Note"
  
  #Formatage des colonnes
  var$PY <- as.numeric(var$PY)
  var$NR <- as.numeric(var$NR)
  var$U2 <- as.numeric(var$U2)
  var$TC <- as.numeric(var$TC)
  var$h <- as.numeric(var$h)
  var$h_so <- as.numeric(var$h_so)
  var$DOM <- as.numeric(var$DOM)
  
  #Traitements des valeurs manquantes
  var <- mutate_at(var, c("NR", "U2",
                          "TC", "h", "h_so",
                          "DOM"), ~replace(., is.na(.), 0)) 
  
  #Suppression colonnes inutiles
  
  var <- subset(var, select = -DB)
  var <- subset(var, select = -RP)
  var <- subset(var, select = -AU_CO)
  
  #Renommage des colonnes
  colnames(var) <- c("Auteurs", "Titre", "Nombre_references",
                     "Annee", "Domaine", "Journal", "Editeur",
                     "ID", "Utilisation_last_180j",
                     "Nombre_citations", "Principal_auteur", "Pays_Auteur_Principal",
                     "h_index_auteur","h_index_source", "Facteur_dominance_auteur",
                     "Note")
  
  #Classement des colonnes
  var<-var[,c("Note", "Titre", "ID", "Principal_auteur", "Auteurs",
              "Annee", "Domaine", "Journal", "Editeur", "Pays_Auteur_Principal",
              "Nombre_references", "Nombre_citations", "Utilisation_last_180j",
              "h_index_auteur", "h_index_source", "Facteur_dominance_auteur")]
  
  return (var)
}

WPCA <- function (X,M){
  
  #On extrait au maximum 3 composantes principales
  if (sum(M)>=3){
    q=3
  }else{
    q=2
  }
  
  #Creation de la matrice de poids des individus
  S<-sum(X$Note)
  N<-diag(X$Note)
  N<-N/S
  
  #Matrice de poids des variables
  M<-diag(M)
  
  #On extrait des variables quantitatives
  V<-X[,c("Annee", "Nombre_references", "Nombre_citations",
          "Utilisation_last_180j","h_index_auteur",
          "h_index_source", "Facteur_dominance_auteur")]
  
  #On applique une transformation en rang fractionnaire
  V<-apply(V, 2, rank, ties.method='average')
  
  #Calcul de la moyenne des variables
  vec1<-rep(1,nrow(V))
  Moy<-t(V)%*%N%*%vec1
  
  #On centre
  V_centr<-V-vec1%*%t(Moy)
  
  #Calcul des 1/ecart type des variables
  VC<-diag(1/sqrt(diag(t(V_centr)%*%N%*%as.matrix(V_centr))))
  
  #On reduit
  V_cr<-as.matrix(V_centr)%*%VC

  #Calcul matrice de correlation ponderee
  C<-t(V_cr)%*%N%*%V_cr%*%M
  
  #On extrait les vecteurs propres
  ev <- eigen(C)
  vectors <- ev$vectors[,1:q]
  
  #On calcule les composantes principales
  Fac<-V_cr%*%M%*%vectors
  colnames(Fac)<-paste(rep("CoordF"), 1:q, sep="")
  
  
  #On joint les composantes principales au Df d'origine 
  if (!("CoordF1"%in%colnames(X)) & !("CoordF2"%in%colnames(X)) & !("CoordF3"%in%colnames(X))){
    X<-cbind(X,Fac)
  }
  
  if ("CoordF1"%in%colnames(X)){
    X$CoordF1<-Fac[,"CoordF1"]
  }
  if("CoordF2"%in%colnames(X)){
    X$CoordF2<-Fac[,"CoordF2"]
  }
  if("CoordF3"%in%colnames(X) & q==2){
    # X<-X[,-c("CoordF3")]
    X <- subset(X, select = -CoordF3)
  }
  if("CoordF3"%in%colnames(X) & q==3){
    X$CoordF3<-Fac[,"CoordF3"]
  }
  if(!("CoordF3"%in%colnames(X)) & q==3){
    X[,"CoordF3"]<-Fac[,"CoordF3"]
  }
  
  
  # val<-ev$values[1:q]
  # Totvar<-sum(ev$values)
  # Dval<-diag(1/sqrt(val))
  # corr<-t(V_cr)%*%N%*%Fac%*%Dval
  # per_var<-val/Totvar
  
  #On calcule les correlation entre variables et composantes principales
  EtFac<-diag(1/sqrt(diag(t(Fac)%*%N%*%Fac)))
  corr<-t(V_cr)%*%N%*%Fac%*%EtFac
  colnames(corr)<-colnames(Fac)
  rownames(corr)<-colnames(V)
  corr<-as.data.frame(corr)
  
  per_var<-NULL
  return (list(X=X,per_var=per_var,corr=corr))
  
}










