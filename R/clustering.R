

#' Calculate cluster memberships for the graph.
#'
#' Calculates the clustering membership for each of the 10 clustering algorithms
#' defined in function \code{\link{getClustering}}
#'
#' @param gg igraph object to cluster
#' @param alg algorithm name
#' @param weights The weights of the edges. It must be a positive numeric
#'        vector, NULL or NA. If it is NULL and the input graph has a ‘weight’
#'        edge attribute, then that attribute will be used. If it is NULL and no such
#'        attribute is present, then the edges will have equal weights. Set
#'        this to NA if the graph has a ‘weight’ edge attribute, but you don't
#'        want to use it for community detection. A larger edge weight means a
#'        stronger connection for this function. The weights value is ignored
#'        for the \code{spectral} clustering.
#'
#' @return data.frame with columns \code{names} and \code{membership}
#' @export
#'
#' @seealso getClustering
#' @examples
#' karate <- make_graph("Zachary")
#' # We need vertex ID in the 'name' attribute of the vertex
#' V(karate)$name<-c(LETTERS,letters)[1:vcount(karate)]
#' m<-calcMembership(karate, 'lec')
#' head(m)
calcMembership <- function(gg,
                           alg = c('lec',
                                   'wt',
                                   'fc',
                                   'infomap',
                                   'louvain',
                                   'sgG1',
                                   'sgG2',
                                   'sgG5',
                                   'spectral'),
                           weights = NULL) {
    ids <- V(gg)$name
    cl <- getClustering(gg, alg,weights=weights)
    if (!is.null(cl)) {
        cc       <- data.frame(names = cl$names,
                               membership = cl$membership)
    } else{
        cc <- data.frame(names = 'names', membership = 0)[FALSE,]
    }
    return(cc)
}

#' Calculate memberships for all clustering algorithms and store them on the
#' graph vertices.
#'
#' This function will call \code{\link{calcClustering}} for each clustering
#' algorithm given in our predefined list. In the event no clustering could be
#' performed, warnings will be issued and no new vertex attribute added to the
#' graph.
#'
#' @param gg graph for analysis
#' @param weights The weights of the edges. It must be a positive numeric
#'        vector, NULL or NA. If it is NULL and the input graph has a ‘weight’
#'        edge attribute, then that attribute will be used. If NULL and no such
#'        attribute is present, then the edges will have equal weights. Set
#'        this to NA if the graph was a ‘weight’ edge attribute, but you don't
#'        want to use it for community detection. A larger edge weight means a
#'        stronger connection for this function. The weights value is ignored
#'        for the \code{spectral} clustering.
#'
#' @return new graph object with all membership results stored as a vertex
#'         attribute.
#' @export
#' @seealso calcClustering
#'
#' @examples
#' g1 <- make_star(10, mode="undirected")
#' V(g1)$name <- letters[1:10]
#' g1<-calcAllClustering(g1)
#' clusteringSummary(g1)
calcAllClustering <- function(gg,weights = NULL) {
    ids <- V(gg)$name
    cnames <- c('ID',
                'lec',
                'wt',
                'fc',
                'infomap',
                'louvain',
                'sgG1',
                'sgG2',
                'sgG5',
                'spectral')
    l <- list()
    l[[cnames[1]]] <- ids
    for (ai in 2:length(cnames)) {
        an <- cnames[ai]
        cm <- calcMembership(gg, an,weights=weights)
        if (dim(cm)[1] > 0) {
            l[[an]] <- as.character(cm$membership)
            mod <- modularity(gg, cm$membership)
            gg <- set_graph_attr(gg, an, mod)
        }
    }
    m <- do.call(cbind, l)
    ggm <- applpMatrixToGraph(gg , m)
    return(ggm)
}

#' Calculate community membership for given clustering algorithm and store the
#' results as new vertex attributes in the graph..
#'
#' When applying resampling the clustering results of a clustering algorithm
#' applied to a graph can differ due to the stochastic nature of the resampling
#' algorithm. To allow reproducible downstream analysis clustering results are
#' stored as vertex attributes in the graph. This function call
#' \code{\link{getClustering}} and stores community membership as new vertex
#' attribute in the graph, and Modularity as a new graph attribute prefix with
#' the \code{alg} name.
#'
#'
#' NOTE: \code{\link{getClustering}} verifies algorithm names with
#' \code{\link[base]{match.arg}} so correct membership will be calculated, but
#' name of the attribute is taken from \code{alg} argument, so it is possible
#' that vertex attribute name won't exactly match name of the algorithm from
#' \code{link{getClustering}}.
#'
#' @param gg igraph object to cluster
#' @param alg algorithm to apply
#' @param weights The weights of the edges. It must be a positive numeric
#'        vector, NULL or NA. If it is NULL and the input graph has a ‘weight’
#'        edge attribute, then that attribute will be used. If NULL and no such
#'        attribute is present, then the edges will have equal weights. Set
#'        this to NA if the graph was a ‘weight’ edge attribute, but you don't
#'        want to use it for community detection. A larger edge weight means a
#'        stronger connection for this function. The weights value is ignored
#'        for the \code{spectral} clustering.
#'
#' @seealso getClustering
#'
#' @return modified igraph object with calculated membership stored as a vertex
#'         attribute and modularity as a graph attribute
#' @export
#'
#' @examples
#' karate <- make_graph("Zachary")
#' # We need vertex ID in the 'name' attribute of the vertex
#' V(karate)$name<-c(LETTERS,letters)[1:vcount(karate)]
#' g<-calcClustering(karate, 'louvain')
#' vertex_attr_names(g)
#' graph_attr(g, 'louvain')
calcClustering <- function(gg, alg,weights = NULL) {
    cl <- getClustering(gg, alg,weights=weights)
    if (!is.null(cl)) {
        ids <- V(gg)$name
        m      <- matrix(NA, ncol = 2, nrow = length(ids))
        colnames(m) <- c('ID', alg)
        m[, 1] <- ids
        m[, 2] <- as.character(cl$membership)
        ggm <- applpMatrixToGraph(gg , m)
        mod <- modularity(ggm, cl$membership)
        ggm <- set_graph_attr(ggm, alg, mod)
        return(ggm)
    } else{
        return(gg)
    }
}

#' Get clustering results for the graph.
#'
#' Wrapper function for calculation of clustering for predefined set of ten
#' algorithms:
#' * lec -- leading eigenvector community (version of
#' \code{\link[igraph]{cluster_leading_eigen}}),
#' directed graph will be converted to undirected by
#' \code{\link[igraph]{as.undirected}} with mode \code{collapse};
#' * wt -- walktrap community \code{\link[igraph]{cluster_walktrap}};
#' * fc -- fastgreedy community \code{\link[igraph]{cluster_fast_greedy}},
#' directed graph will be converted to undirected by
#' \code{\link[igraph]{as.undirected}} with mode \code{collapse};
#' * infomap -- infomap community \code{\link[igraph]{cluster_infomap}};
#' * louvain -- cluster_louvain \code{\link[igraph]{cluster_louvain}},
#' directed graph will be converted to undirected by
#' \code{\link[igraph]{as.undirected}} with mode \code{collapse};
#' * sgG1 -- spin-glass model and simulated annealing clustering (version of
#' \code{\link[igraph]{cluster_spinglass}} with spins=500 and gamma=1);
#' * sgG2 -- spin-glass model and simulated annealing clustering (version of
#' \code{\link[igraph]{cluster_spinglass}} with spins=500 and gamma=2);
#' * sgG5 -- spin-glass model and simulated annealing clustering (version of
#' \code{\link[igraph]{cluster_spinglass}} with spins=500 and gamma=7);
#' * spectral -- spectral modularity clustering
#' \code{\link[rSpectral]{spectral_igraph_communities}};
#'
#' graph suppose to be undirected. If algorithm failed warning will be issued
#' and function returned NULL.
#'
#' Algorithm names are verified with \code{\link[base]{match.arg}}.
#'
#' @md
#'
#' @param gg igraph object to cluster
#' @param alg clustering algorithm name
#' @param weights The weights of the edges. It must be a positive numeric
#'        vector, NULL or NA. If it is NULL and the input graph has a ‘weight’
#'        edge attribute, then that attribute will be used. If NULL and no such
#'        attribute is present, then the edges will have equal weights. Set
#'        this to NA if the graph was a ‘weight’ edge attribute, but you don't
#'        want to use it for community detection. A larger edge weight means a
#'        stronger connection for this function. The weights value is ignored
#'        for the \code{spectral} clustering.
#'
#' @return \code{\link[igraph]{communities}} object or NULL if algorithm failed.
#' @export
#' @importFrom rSpectral spectral_igraph_communities
#'
#' @examples
#' data(karate,package='igraphdata')
#' c<-getClustering(karate,'lec')
#' c$modularity
getClustering <- function(gg,
                          alg = c('lec',
                                  'wt',
                                  'fc',
                                  'infomap',
                                  'louvain',
                                  'sgG1',
                                  'sgG2',
                                  'sgG5',
                                  'spectral'),
                          weights = NULL) {
    alg <- match.arg(alg)
    #TODO: make a proper fix of disconnected graph clustering
    #c<-components(gg)
    if(vcount(gg)==1){
        algname<- switch(
            alg,
            lec = "leading eigenvector",
            wt = 'walktrap',
            fc = "fast greedy",
            infomap = 'infomap',
            louvain = "multi level",
            sgG1 = "spinglass",
            sgG2 = "spinglass",
            sgG5 = "spinglass",
            spectral = "spectral"
        )
        cl<-make_clusters(gg,membership = c(1),algorithm = algname)
        return(cl)
    }else{
    lec <- function(gg) {
        ugg <- as.undirected(gg,mode = 'collapse')
        lec     <- igraph::cluster_leading_eigen(ugg,weights=weights)
        ll      <-
            igraph::cluster_leading_eigen(ugg, start = membership(lec),
                                                  weights=weights)
    }
    cl <- try(switch(
        alg,
        lec = lec(gg),
        wt = igraph::cluster_walktrap(gg,weights=weights),
        fc = igraph::cluster_fast_greedy(as.undirected(gg,mode = 'collapse'),
                                          weights=weights),
        infomap = igraph::cluster_infomap(gg,e.weights=weights),
        louvain = igraph::cluster_louvain(as.undirected(gg,mode = 'collapse'),
                                          weights=weights),
        sgG1 = igraph::cluster_spinglass(gg,
                                           spins = as.numeric(500),
                                           weights=weights, gamma = 1),
        sgG2 = igraph::cluster_spinglass(gg,
                                           spins = as.numeric(500),
                                           weights=weights, gamma = 2),
        sgG5 = igraph::cluster_spinglass(gg,
                                           spins = as.numeric(500),
                                           weights=weights, gamma = 5),
        spectral = rSpectral::spectral_igraph_communities(gg)
    ),silent = TRUE)
    if (inherits(cl, "try-error")) {
        warning('Clustering calculations for algorithm "',
                alg,
                '" failed. NULL is returned')
        return(NULL)
    }
    return(cl)
    }
}

#' Matrix of cluster characteristics
#'
#' Function to calculate basic summary statistics after apply clustering
#' algorithm:
#' * N -- number of vertices in the graph \code{\link[igraph]{vcount}}
#' * mod -- clustering modularity \code{\link[igraph]{modularity}}, the ratio
#' of edges found within communities to the number of edges found between
#' communities, relative to a randomised model
#' * C -- number of clusters
#' * Cn1 -- number of singletones (clusters of size 1)
#' * Cn100 -- number of clusters containing more than 100 nodes
#' * mu -- the ratio of edges found within communities to the number of edges
#' found between communities
#' * Min. C -- minimum of the cluster size
#' * 1st Qu. C -- first quartile of the cluster size
#' * Median C -- median of the cluster size
#' * Mean C -- average cluster size
#' * 3rd Qu. C  -- third quartile of the cluster size
#' * Max. C -- maximum of the cluster size
#'
#' @param gg graph to analyse
#' @param att vector of attribute names that contains membership data
#'
#' @return matrix of clustering characteristics
#' @export
#' @md
#'
#' @examples
#' data(karate,package='igraphdata')
#' g<-calcAllClustering(karate)
#' clusteringSummary(g)
clusteringSummary <- function(gg,
                              att = c('lec',
                                      'wt',
                                      'fc',
                                      'infomap',
                                      'louvain',
                                      'sgG1',
                                      'sgG2',
                                      'sgG5',
                                      'spectral')) {
    attN <- vertex_attr_names(gg)
    N <- vcount(gg)
    idx <- match(attN, att)
    clusterings <- attN[!is.na(idx)]
    res <- list()
    for (c in clusterings) {
        cmem <- as.numeric(vertex_attr(gg, c))
        mod <- modularity(gg, cmem)
        Cn <- table(cmem)
        C <- length(Cn)
        Cn1 <- length(which(Cn == 1))
        Cn100 <- length(which(Cn >= 100))
        summary(as.vector(Cn)) -> s
        names(s) <- paste(names(s), 'C')
        sgraphs <-
            lapply(names(Cn),
                   getClusterSubgraphByID,
                   gg = gg,
                   mem = cmem)
        ug <- disjoint_union(sgraphs)
        mu <- 1 - ecount(ug) / ecount(gg)
        r1 <- c(N,mod, C, Cn1, Cn100, mu)
        names(r1) <- c('N','mod', 'C', 'Cn1', 'Cn100', 'mu')
        res[[c]] <- c(r1, s)
    }
    return(makeDataFrame(do.call(rbind, res)))
}
