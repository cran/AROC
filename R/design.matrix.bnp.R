design.matrix.bnp <-
function(formula, data) {
	iform <- interpret.AROCformula(formula, data)
	data.cov <- iform$data.cov
	X <- NULL
	Xterms <- list()
	for(i in 1:iform$npartial) {
		if(any(iform$II[,i] == -1)) {
			if(iform$h[i] == 0 | iform$h[i] == 1) { # Linear and factor
				#X <- cbind(X, data.cov[,iform$II[2,i]])
				#Xterms[[i]] <- "linear"
				mf <- model.frame(paste0("~", colnames(data.cov[,iform$II[2,i],drop = FALSE])), data.cov, drop.unused.levels = TRUE)
				mt <- terms(mf)
				Xterms[[i]] <- mt
				X <- cbind(X, model.matrix(mt, mf)[,-1, drop = FALSE]) # Here we delete the intercept
			} else {
				Bs <- bbase.os(data.cov[,iform$II[2,i]], K = iform$K[i], intercept = FALSE)
				Xterms[[i]] <- Bs
				X <- cbind(X, Bs)
			}
		} else { # Factor by curve
			Bs <- bbase.interaction.factor.by.curve.os(data.cov[,iform$II[2,i]], data.cov[,iform$II[1,i]], K = iform$K[i])
			Xterms[[i]] <- Bs
			X <- cbind(X, Bs)
		}
	}
	# Add the intercept
	X <- cbind(1, X)
	res <- list()
	res$X <- X
	res$terms <- Xterms
	res$iformula <- iform
	class(res) <- "design.matrix.bnp"
	res
}
