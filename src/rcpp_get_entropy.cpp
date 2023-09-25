#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double rcpp_get_entropy(NumericVector &x, std::string base = "log2") {
        x = x / sum(x);
        double result = 0.0;
        for(int i = 0; i < x.size(); i++){
                if(x[i] > 0.0){
                        result -= x[i] * log(x[i]);
                }
        }
        if (base == "log10"){
                result = result / log(10.0);
        } else if (base == "log2"){
                result = result / log(2.0);
        }
        return(result);
}

/*** R
rcpp_get_entropy(c(5, 9), "log2")
rcpp_get_entropy(c(5, 9), "log")
buys = c("no", "no", "yes", "yes", "yes", "no", "yes", "no", "yes", "yes", "yes", "yes", "yes", "no")
freqs = table(buys)/length(buys)
-sum(freqs * log2(freqs))
*/
