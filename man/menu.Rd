\name{menu}
\alias{menu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%% menu rmaeng2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
menu(choices, graphics = FALSE, title = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{choices}{
%%     ~~Describe \code{choices} here~~
}
  \item{graphics}{
%%     ~~Describe \code{graphics} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (choices, graphics = FALSE, title = NULL) 
{
    if (!interactive()) 
        stop("menu() cannot be used non-interactively")
    if (isTRUE(graphics)) {
        if (.Platform$OS.type == "windows" || .Platform$GUI == 
            "AQUA" || (capabilities("tcltk") && capabilities("X11") && 
            suppressWarnings(tcltk::.TkUp))) {
            res <- select.list(choices, multiple = FALSE, title = title, 
                graphics = TRUE)
            return(match(res, choices, nomatch = 0L))
        }
    }
    nc <- length(choices)
    if (length(title) && nzchar(title[1L])) 
        cat(title[1L], "\n")
    op <- paste0(format(seq_len(nc)), ": ", choices)
    if (nc > 10L) {
        fop <- format(op)
        nw <- nchar(fop[1L], "w") + 2L
        ncol <- getOption("width")\%/\%nw
        if (ncol > 1L) 
            op <- paste0(fop, c(rep("  ", min(nc, ncol) - 1L), 
                "\n"), collapse = "")
    }
    cat("", op, "", sep = "\n")
    repeat {
        ind <- .Call(C_menu, as.character(choices))
        if (ind <= nc) 
            return(ind)
        cat(gettext("Enter an item from the menu, or 0 to exit\n"))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
