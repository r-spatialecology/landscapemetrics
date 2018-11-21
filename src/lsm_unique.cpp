#include "lsm_unique.h"
using namespace Rcpp;

std::vector<int> lsm_unique(const Rcpp::IntegerVector &x)
{
    std::unordered_set<int> tab(x.begin(), x.end());
    std::vector<int> classes(tab.begin(), tab.end());

    return classes;
}
