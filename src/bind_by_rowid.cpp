#include <Rcpp.h>

using namespace Rcpp;

SEXP bind_by_rowid_cpp_matches(SEXP x_dt,
                               SEXP y_dt,
                               SEXP matches_dt,
                               const CharacterVector& overlap);

// [[Rcpp::export(rng = false)]]
SEXP bind_by_rowid_cpp(SEXP x_dt,
                       SEXP y_dt,
                       const IntegerVector& x_idx,
                       const IntegerVector& y_idx,
                       const CharacterVector& overlap) {
  List matches = List::create(
    Named("x") = x_idx,
    Named("y") = y_idx
  );
  matches.attr("class") = CharacterVector::create("data.frame");
  matches.attr("row.names") = IntegerVector::create(NA_INTEGER, -x_idx.size());

  return bind_by_rowid_cpp_matches(x_dt, y_dt, matches, overlap);
}
