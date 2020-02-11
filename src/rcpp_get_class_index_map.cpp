#include "rcpp_get_class_index_map.h"

std::map<int, unsigned> get_class_index_map(const std::vector<int> &classes)
{
    std::map<int, unsigned> class_index;
    for (unsigned i = 0; i < classes.size(); i++) {
        class_index.insert(std::make_pair(classes[i], i));
    }
    return class_index;
}
