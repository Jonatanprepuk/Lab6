// TO compile run Rcpp::compileAttributes()
// Then:
// devtools::document()
// devtools::load_all()

// TEMPLATE CODE.
#include <Rcpp.h>
#include <numeric> 
#include <list>
using namespace Rcpp;

// THIS documentation will be exported with roxygen2

// TODO fix documentation here and in R file.

//' Add two integers using C++
//'
//' @param x First integer.
//' @param y Second integer.
//' @return The sum of x and y.
//' @export
// [[Rcpp::export]]
List fast_greedy_knapsack(NumericVector v, NumericVector w, int W){
    
    NumericVector v_w = v / w;
    std::vector<int> idx(v_w.size());
    std::iota(idx.begin(), idx.end(), 0);
    std::sort(idx.begin(), idx.end(), [&v_w](int a, int b){
        return v_w[a] > v_w[b];
    });

    int s2_idx = -1;
    double totalWeight = 0;
    double totalValue = 0;
    std::list<int> elements;

    for(size_t i = 0; i < idx.size(); i++){
        if(w[idx[i]] > W) continue;
        totalWeight += w[idx[i]];

        if(totalWeight > W){
            s2_idx = i;
            break;
        }
        totalValue += v[idx[i]];
        elements.push_back(idx[i] + 1);
    }

    if(s2_idx != -1 && totalValue < v[idx[s2_idx]]){
        int s2Element = idx[s2_idx] + 1;
        return List::create(
            Named("value")    = v[idx[s2_idx]],
            Named("elements") = s2Element
        );
    } else if(elements.size() == 0){
        return List::create(
            Named("value") = 0.0,
            Named("elements") = LogicalVector::create(NA_LOGICAL) 
        );
    }

    return List::create(
    Named("value")    = totalValue,
    Named("elements") = elements 
  );
}