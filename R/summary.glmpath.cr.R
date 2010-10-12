summary.glmpath.cr <-
function (object, ...) 
{
    ii <- object$new.A
    ii[length(ii)] <- TRUE
    M <- data.frame(Df = object$df[ii], Deviance = object$deviance[ii], 
        AIC = object$aic[ii], BIC = object$bic[ii])
    dimnames(M)[[1]] <- paste("Step", which(ii), sep = " ")
    M
}

