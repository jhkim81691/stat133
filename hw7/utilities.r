library(RUnit)

YOUR.CODE.HERE <- NULL

getArg <- function(arg) {
    toString(as.expression(match.call(sys.function(-1), call=sys.call(-1), expand.dots=FALSE)[[arg]]))
}

test <- function(a,b, ...) {
    char.a <<- getArg("a")
    char.b <<- getArg("b")
    result <- all.equal(a, b, ...)
    if(!identical(result, TRUE)) {
        if(toString(a) == toString(b)) {
            stop(paste0("Although\n",
                        char.a, " = ", toString(a),
                        "\nand\n",
                        char.b, " = ", toString(b),
                        "\nlook the same, there's something different in their structure.",
                        "\nYou might want to take a look at structure using \"dput\": ",
                        "\ni.e. dput(", char.a, ") vs dput(", char.b, ")",
                        "\nThe error message given was:\n",
                        result))
        }
        stop(paste0("I expected ", toString(char.a), " to return:\n",
                   toString(b),
                   "\nBut instead I got:\n",
                   toString(a)))
    }
    else invisible(print(paste0(char.a, " == ", char.b, " PASSED")))
}

alphacol <- function(col, alpha) {
    do.call(rgb, c(as.list(col2rgb(col)/255), alpha))
}
