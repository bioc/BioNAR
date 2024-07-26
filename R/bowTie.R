btgroups<-c('SCC', 'IN', 'OUT', 'TU', 'IDR', 'ODR', 'OTR' )
#' Calculates bow-tie decomposition and marks vertices with one of the
#' following in the \code{BowTie} attribute:
#' \itemize{
#'   \item SCC -- maximal strong connected component;
#'   \item IN -- vertices not in SCC, but SCC is reachable from them;
#'   \item OUT -- vertices not in SCC, but reachable from SCC;
#'   \item TU -- vertices not in all three above, but reachable from IN and
#'   OUT is reachable from them (TUBES);
#'   \item IDR -- vertices not in SCC, but they are reachable from IN and
#'   OUT is NOT reachable from them (INTENDRILS);
#'   \item ODR -- vertices not is SCC, but they are NOT reachable from IN and
#'   OUT is reachable from them (OUTTENDRILS);
#'   \item OTR -- all other vertices.
#' }
#'
#' Algorithm proposed in:
#'
#' "Bow-tie Decomposition in Directed Graphs" - Yang et al. IEEE (2011)
#'
#' @param g graph to analyse
#'
#' @return graph with \code{BowTie} vertex attribute
#' @export
#'
markBowTie<-function(g){
    glcc<-findLCC(g)
    scc<-components(glcc,mode='strong')
    sccV<-V(glcc)$name[which(scc$membership==which.max(scc$csize))]
    rglcc<-reverse_edges(glcc)
    glcc_dfs<-dfs(glcc, which(scc$membership==which.max(scc$csize))[1],
                  mode='out',unreachable = FALSE,dist=TRUE)
    rglcc_dfs<-dfs(rglcc, which(scc$membership==which.max(scc$csize))[1],
                   mode='out',unreachable = FALSE,dist=TRUE)
    idx<-match(names(rglcc_dfs$order), sccV)
    inV<-names(rglcc_dfs$order[is.na(idx)])
    idx<-match(names(glcc_dfs$order), sccV)
    outV<-names(glcc_dfs$order[is.na(idx)])
    idx<-which( !V(glcc)$name %in% c(sccV,inV,outV))
    restV<-V(glcc)$names[idx]
    idrV<-c()
    odrV<-c()
    tuV<-c()
    otrV<-c()
    for(i in idx[!is.na(idx)]){
        rdfs<-dfs(rglcc,i,mode='out',unreachable = FALSE,dist=TRUE)
        fdfs<-dfs(glcc,i,mode='out',unreachable = FALSE,dist=TRUE)
        vro <- length(which(!is.na(match(names(fdfs$order),outV))))
        irv <- length(which(!is.na(match(names(rdfs$order),inV))))
        if(irv > 0 & vro > 0){
            tuV<-c(tuV,V(glcc)$name[i])
        }else if(irv > 0 & vro == 0){
            idrV<-c(idrV,V(glcc)$name[i])
        }else if(irv == 0 & vro > 0){
            odrV<-c(odrV,V(glcc)$name[i])
        }else{
            otrV<-c(otrV,V(glcc)$name[i])
        }
        #cat(i,'(',irv,vro,')\n')
    }
    l<-list()
    if(length(sccV)>0){
        l[['SCC']]<-data.frame(ID=sccV,val='SCC')
    }
    if(length(inV)>0){
        l[['IN']]<-data.frame(ID=inV,val='IN')
    }
    if(length(outV)>0){
        l[['OUT']]<-data.frame(ID=outV,val='OUT')
    }
    if(length(tuV)>0){
        l[['TU']]<-data.frame(ID=tuV,val='TU')
    }
    if(length(idrV)>0){
        l[['IDR']]<-data.frame(ID=idrV,val='IDR')
    }
    if(length(odrV)>0){
        l[['ODR']]<-data.frame(ID=odrV,val='ODR')
    }
    if(length(otrV)>0){
        l[['OTR']]<-data.frame(ID=otrV,val='OTR')
    }
    adf<-do.call(rbind,l)
    ggm <- annotateVertex(g,'BowTie',adf)
    return(ggm)
}

