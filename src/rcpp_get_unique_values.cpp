#include "rcpp_get_unique_values.h"
using namespace Rcpp;

std::vector<int> rcpp_get_unique_values(const Rcpp::IntegerVector &x, bool na_omit)
{
    std::set<int> s;
    unsigned size = x.size();
    for(unsigned i = 0; i < size; i++) {
        s.insert(x[i]);
    }

    std::vector<int> classes(s.begin(), s.end());

    if (na_omit) {
        const int na = NA_INTEGER;
        if (classes[0] == na)
            classes.erase(classes.begin() + 0);
    }
    return classes;
}
