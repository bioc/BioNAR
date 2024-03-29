#require("minpack.lm")
#require(cowplot)

### Sigmoid function ### create a function to generate sigmoid pattern
sigmoid <- function(pars, xx){

    a <- as.numeric(pars[1])#lower asymptote,          ideal == 0
    b <- as.numeric(pars[2])#upper asymptote,          ideal == 1
    c <- as.numeric(pars[3])#gradiant, rate, or slope, ideal == -2
    d <- as.numeric(pars[4])#inflextion point,         ideal == median(xx) = 3

    return( a + ((b-a)/(1+exp(-c*(xx-d)))) )
}

residFun <- function(pars, observed, xx) observed - sigmoid(pars,xx)

storeFitInfo <- function( ALG, LL ){

    tmp <- c(ALG)

    for( i in seq_along(LL) ){
        tmp <- cbind(tmp, as.vector(LL)[i])
    }

    return(tmp)

}

storeParInfo <- function( ALG, LL ){

    tmp <- c(ALG)

    for( i in seq_along(LL[,1]) ){
        tmp <- cbind(tmp, LL[i,1], LL[i,2])
    }

    return(tmp)

}

highlightRate <- function( rates, val=-2){

    indx <- which(rates==val)
    if( length(indx) == 0 ){
        indx <- -1
    }

    return(indx)

}


#' Plot results of the sigmoid fit
#'
#' @param x steps along the Fe
#' @param rates parameters of the sigmoid
#' @param model fitted model
#' @param alg name of the clustering algorithm
#' @param pv Kolmogorov-Smirnov test's p-value
#'
#' @return \code{\link[ggplot2]{ggplot}} object with sigmoid fit plot
#' @export
plotSigmoid <- function( x, rates, model, alg="", pv=0 ){

    conf     <- NULL
    conf <- try(confint(model), silent = TRUE)

    if( !inherits(conf,'try-error') ){
        # adding the 95% confidence interval around the fitted coefficient
        lower <- list(a=conf[1, 1], b=conf[2, 1], c=conf[3, 1], d=conf[4, 1])
        upper <- list(a=conf[1, 2], b=conf[2, 2], c=conf[3, 2], d=conf[4, 2])
    }

    ## fitted values
    y    <- model$m$lhs()
    yhat <- as.vector(fitted(model))

    if( !inherits(conf,'try-error') ){
        ylower <- sigmoid(pars=lower, xx=x)
        yupper <- sigmoid(pars=upper, xx=x)
    } else {
        ylower <- rep(0, length(y))
        yupper <- rep(0, length(y))
    }
    ##---

    ##--- data.frame to plot
    df <- cbind( rep(alg,length(x)), x, y, yhat, ylower, yupper )

    ##--- 'ideal' sigmoid curves, changing the rate values
    R <- length(rates)
    Rsize <- rep(1,R)
    Rcol  <- rep("grey50",R)
    indx  <- highlightRate( rates=rates, val=-2 )
    if( indx != -1 ){
        Rsize[indx[1]] <- 2
        Rcol[indx[1]]  <- "black"
    }
    for( r in seq_len(R) ){
        pp <- list(a=0, b=1, c=rates[r], d=round(median(x)) )
        yi <-  sigmoid(pars=pp, xx=x)
        df <- cbind(df,yi)
    }
    ##---

    ##--- data.frame to plot
    colnames(df) <- c("alg", "x", "y", "yhat", "ylower", "yupper",
                      sprintf("yiR%.0f", seq(1,R,1)))
    #cat(format(Sys.time(), "%b %d %X"),colnames(df),'\n')
    df <- as.data.frame(df)

    ##--- labels
    qq   <- as.vector(quantile(as.numeric(df$x)))
    xval <- qq
    xlab <- as.character(qq)

    ylow <- ifelse( min(y) < 0, min(y), 0)
    yup  <- ifelse( max(y) > 1, max(y), 1)
    ##---

    ##--- should we plot the CI
    plotCI <- TRUE
    if( is.null(conf) ){ plotCI <- FALSE }

    ##--- p.value from KS test for model against 'ideal' sigmoid
    ## with rate value = -2
    pv <- as.numeric(pv)
    if( is.na(pv) ){ pv <- 0 }
    ##---

    ##--- build the plot
    gplot <- ggplot(df, aes(as.numeric(x)))+
        geom_point(aes(y=as.numeric(as.vector(y))),   shape=1, size=2.5)+
        geom_line(aes(y=as.numeric(as.vector(yhat))), linetype="dashed",
                  color="red", size=2)+
        geom_line(aes(y=as.numeric(as.vector(yiR1))), linetype="solid",
                  color=Rcol[1], size=Rsize[1])+
        geom_line(aes(y=as.numeric(as.vector(yiR2))), linetype="solid",
                  color=Rcol[2], size=Rsize[2])+
        geom_line(aes(y=as.numeric(as.vector(yiR3))), linetype="solid",
                  color=Rcol[3], size=Rsize[3])+
        geom_line(aes(y=as.numeric(as.vector(yiR4))), linetype="solid",
                  color=Rcol[4], size=Rsize[4])+
        geom_line(aes(y=as.numeric(as.vector(yiR5))), linetype="solid",
                  color=Rcol[5], size=Rsize[5])+
        #geom_line(aes(y=as.numeric(as.vector(df$yiR6))), linetype="solid",
        #color=Rcol[6], size=Rsize[6])+
        {if(plotCI)geom_line(aes(y=as.numeric(as.vector(ylower))),
                             linetype="dashed", color="blue", size=2)}+
        {if(plotCI)geom_line(aes(y=as.numeric(as.vector(yupper))),
                             linetype="dashed", color="blue", size=2)}+
        labs(x="log2(Fe)",y="Fraction of Enriched Communities",
             title=sprintf("%s, KS GoF = %3.e", alg, pv))+
        theme(axis.title.x=element_text(face="bold",size=rel(1.1)),
              axis.title.y=element_text(face="bold",size=rel(1.1)),
              legend.text=element_text(face="bold",size=rel(1.5)),
              plot.title=element_text(face="bold",size=rel(1.5)),
              legend.position="bottom")+
        coord_cartesian(xlim = c(min(x), max(x)), ylim=c(ylow, yup))+
        #scale_y_continuous(expand=c(0,0),limits=c(ylow,yup))+
        #scale_x_discrete(expand=c(0,0), limit=xval, labels=xlab)+
        theme(panel.grid.major = element_line(colour = "grey40"),
              panel.grid.minor = element_line(colour="grey40",size=0.1),
              panel.background = element_rect(fill = "white"),
              panel.border = element_rect(linetype="solid",fill=NA))+
        guides(color = 'none',
               alpha = 'none',
               size  = 'none')
    ##---

    return(list(gplot=gplot, df=df))
}

addNoise <- function( Y, MN=0, SD=0.05 ){
    return( Y+rnorm(length(Y),mean=MN, sd=SD) )
}


##goodness of fit test, KS
#' Goodnes of fit KS test
#'
#' This is internal function and do not suppose to be called by user.
#'
#' @param x steps along the Fe
#' @param rate parameters of the sigmoid
#' @param model fitted model
#' @param sigma2 noise strength
#' @param countDATA should points to be counted
#'
#' @return list of \code{\link[stats]{ks.test}} values for each value in
#'         \code{rate}
#' @export
gofs <- function(x, rate, model, sigma2=NULL, countDATA=TRUE ){

    y    <- model$m$lhs()
    yhat <- fitted(model)

    R  <- length(rate)
    KS <- list()

    for( r in seq_len(R) ){

        pp <- list(a=0, b=1, c=rate[r], d=round(median(x)) )
        yi <- sigmoid(pars=pp, xx=x)

        KS[[r]]      <- ks.test(yhat, yi)
        names(KS)[r] <- sprintf("rate_%.1f", rate[r])
    }

    return(KS)

}

#' Fit Fold-enrichment distribution to sigmoid function
#'
#' This function calculates fit of the Fold-Enrichment distribution to the
#' sigmoid function with the levels of noise specidied in \code{SDV} for all
#' clustering algorithms, which have non-zero \code{SUM3$`Psig&ORsig`} in the
#' enrichment table summary results. The function returns
#' the list in which each element contains result for one of the noise level.
#'
#' Results are repersented as a list with five elements:
#' * gridplot that allow comparison of fitting for different clustering
#'   algorithms;
#' * plots the list of individual plots from gridplot;
#' * fitInfo the data.frame that contains results of fitting, such as
#'   message, number of iterations and exit code;
#' * parInfo values and standard deviations for all sigmoid parameters;
#' * ks table of Kolmogorov-Smirnov test p-values.
#'
#' Grid plot is designed in a way to be viewed in the device at least 12 inches
#' in width and 12 inches in height.
#'
#' @param stat enrichment results obtained from \code{\link{summaryStats}}
#' @param SDv vector of noise SD values
#'
#' @return list of fitted functions tables and plots
#'
#' @importFrom minpack.lm nls.lm.control nlsLM
#' @importFrom cowplot plot_grid
#' @importFrom stats confint fisher.test fitted ks.test median
#' @importFrom stats p.adjust quantile rnorm
#' @seealso [summaryStats()]
#' @export
fitSigmoid<-function(stat,SDv=c(0, 0.05, 0.1, 0.5)){
    df<-stat$SUM3
    x.range.value  <- "6.0"
    Xmax <- match(x.range.value,colnames(df))
    if(is.na(Xmax)){
        Xmax<-ncol(df)
    }
    tt   <- df[,seq_len(Xmax)]
    colnames(tt) <- colnames(df)[seq_len(Xmax)]
    N <- length(colnames(tt))
    x <- as.numeric(colnames(tt)[3:N])
    SDlab <- as.character(SDv)#c("0","0.05","0.1","0.5")

    ## test fit against different 'idealised' sigmoid curves
    ## using KS gof statistic
    rates <- c(-10, -5, -2, -1, -0.5)

    resout<-list()
    CNfit <- c("alg", "isConv", "finIter", "finTol",
               "stopCode", "stopMessage")
    errorFitInfo<-c("No", "NA", "NA",
                    "NA")
    for( s in seq_along(SDv) ){

        models <- list()
        GPLOTS <- list()
        gof    <- list()

        ## save the fit infomation for each run
        #fitInfo          <- matrix("", ncol=length(CNfit), nrow=length(tt[,1]))
        #colnames(fitInfo) <- CNfit
        #fitInfo[,1]       <- tt[,1]

        fitInfo <- data.frame()
        parInfo <- data.frame()

        for( i in seq_along(tt[,1]) ){

            if(as.numeric(tt[i,2])>0){
                y <- as.numeric(tt[i,3:N])/as.numeric(tt[i,2])
                y <- BioNAR:::addNoise(y, SD=SDv[s])#add gaussian noise to our data

                ## starting parameter values for fit
                pp <- list(a=0, b=round(max(y)), c=-2, d=round(median(x)) )

                ## fit
                m.s <- try(minpack.lm::nlsLM(y ~ a + ((b - a)/(1 + exp(-c * (x - d)))),
                                             start = pp, trace = FALSE,
                                             control=minpack.lm::nls.lm.control(
                                                 maxiter = 150)),silent=TRUE)

                if( !inherits(m.s,'try-error') ){
                    # models[[i]]      <- m.s
                    # names(models)[i] <- as.character(tt[i,1])
                    models[[as.character(tt[i,1])]]      <- m.s

                    fitInfo <- rbind(fitInfo,
                                     storeFitInfo( as.character(tt[i,1]),
                                                   unlist(models[[as.character(tt[i,1])]]$convInfo) ) )

                    parInfo <- rbind(parInfo,
                                     storeParInfo( as.character(tt[i,1]),
                                                   unlist(summary(models[[as.character(tt[i,1])]])$parameters[,c(1,2)]) ))
                }else{
                    fitInfo <- rbind(fitInfo,
                                     storeFitInfo( as.character(tt[i,1]),
                                                   c(errorFitInfo,as.character(m.s))) )
                    parInfo <- rbind(parInfo,
                                     storeParInfo( as.character(tt[i,1]),
                                                   matrix(NA,nrow = 4,ncol=2)))

                }
                rm(m.s)

            }

        }

        ##---save the fit infomation
        colnames(fitInfo) <- CNfit
        fitInfo$finIter<-as.numeric(fitInfo$finIter)
        fitInfo$finTol<- as.numeric(fitInfo$finTol)
        # write.table(fitInfo, sprintf("%s/FitInfo_sd_%s.csv",plotDIR,SDlab[s]),
        #             sep="\t", col.names=T, row.names=F, quote=F)

        colnames(parInfo) <- c("alg",
                               c(rbind(
                                   names(models[[1]]$m$getPars()),
                                   sprintf("sd_%s",
                                           names(models[[1]]$m$getPars())))))
        # write.table(parInfo, sprintf("%s/parInfo_sd_%s.csv",plotDIR,SDlab[s]),
        #             sep="\t", col.names=T, row.names=F, quote=F)

        ##---print KS test results for each rate and noisy study here
        CN <- c("alg", sprintf("Rate_%.1f",rates))
        oo <- matrix("", ncol=length(CN), nrow=length(names(models)))
        colnames(oo) <- CN
        oo[,1] <- names(models)

        for( i in seq_along(names(models)) ){

            ks   <- BioNAR:::gofs(x, rates, models[[i]])
            indx <- BioNAR:::highlightRate( rates=rates, val=-2 )
            PV <- as.numeric(ks[[1]]$p.value)
            if( indx != -1 ){
                PV <- as.numeric(ks[[indx[1]]]$p.value)
            }
            tmp <- BioNAR:::plotSigmoid( x=x, rates=rates, model=models[[i]],
                                         alg=names(models)[i], pv=PV)

            gof[[i]]    <- ks
            GPLOTS[[i]] <- tmp$gplot

            names(gof)[i]    <- names(models)[i]
            names(GPLOTS)[i] <- names(models)[i]

            for(j in seq_along(rates)){
                oo[i,(j+1)] <- ks[[j]]$p.value
            }
        }

        p <- cowplot::plot_grid(plotlist=GPLOTS,
                                labels = "AUTO", label_size=20,
                                label_fontface="bold")
        # ggsave(sprintf("%s/Fitting_%s.png",plotDIR,SDlab[s]), p,
        #        width=20, height=20, device="png")
        #
        # write.table(oo, sprintf("%s/ks_pv_sd_%s.csv",plotDIR, SDlab[s]),
        #             sep="\t", col.names=T, row.names=F, quote=F)
        ooks<-as.data.frame(oo)
        ooks[-1]<-lapply(ooks[-1],as.numeric)
        pinf<-as.data.frame(parInfo)
        pinf[-1]<-lapply(pinf[-1],as.numeric)
        resout[[SDlab[s]]]<-list(gridplot=p,
                                 plots=GPLOTS,
                                 fitInfo=fitInfo,
                                 parInfo=pinf,
                                 ks=ooks)
        rm(models, GPLOTS, gof)

    }
    return(resout)
}
