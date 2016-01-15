#' Given amino acid sequence/s in one of several formats, replaces N 
#' (asparagine) with an O character to depict potential N-linked glycosylation 
#' (PNG) motifs.
#'
#' NB: Assumes without testing that the input is amino acids, not DNA.
#'
#' The PNG motif is NxS or NxT, where x is any amino acid except
#' proline.  Skips gap characters (taken to be the dash or hyphen, i.e. '-').
#' 
#' @param x an input character string or matrix of aligned amino-acid characters.
#'
#' @return Output type should match input type, with Ns in PNG motifs renamed to O.
#'
#' @examples
#' lassie::pngs2o("NNST-N-PN-S-T")
#' lassie::pngs2o(letters)
#' lassie::pngs2o("no png sites here")
#'
#' @export
pngs2o <- function(x=NULL) {

    if (is.null(x))
        x

    if (is.vector(x) & class(x) == "character") {

        if (length(x) == 1) { # one string

            if (nchar(x) < 3)
                return (x)

            # split input 
            S <- seqinr::s2c(x)

            # Will Fischer says the (?=blah) is lookahead syntax.  Thanks Will!
            png.sites = gregexpr("N(?=-*[^P]-*[ST])", x, perl=T, ignore.case=T)

            # if no matches were found, png.sites[[1]] will be == -1.
            # to execute the substitution without testing for this
            # would shorten the sequences by removing the first site.
            if (all(png.sites[[1]] > 0))
                S[png.sites[[1]]] = "O"

            t = seqinr::c2s(S)

            if (!is.null(names(x)))
                names(t) = names(x)

            x = t

        } else {

            if (all(nchar(x) == 1))
                x = seqinr::s2c(lassie::pngs2o(seqinr::c2s(x)))
            else
                x = sapply(1:length(x), function(i) 
                        lassie::pngs2o(x[i]))
        }

    } else if (is.matrix(x)) {

        for (i in 1:nrow(x))
            x[i, ] = seqinr::s2c(lassie::pngs2o(seqinr::c2s(x[i, ])))

#        return ( x )

    } else if (is.list(x) & class(x) != "alignment") {

        for (i in 1:length(x))
            if (class(x[[i]]) == "SeqFastaAA")
                x[[i]] = pngs2o(x[[i]])

#        return ( x )

    } else if (class(x) == "SeqFastaAA") {

        x <- seqinr::s2c(pngs2o(seqinr::c2s(x)))

    } else if (class(x) == "alignment") {

        x$seq = sapply(1:x$nb, function(i)
            pngs2o(x$seq[[i]]))

#        return ( x )

    } #else {

    return ( x )
#    }
}
