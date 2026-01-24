#include <Rcpp.h>
#include <unordered_set>
#include <string>

using namespace Rcpp;

inline bool is_factor(SEXP x) {
  return Rf_isFactor(x);
}

inline SEXP fill_by_index_str(SEXP col, const IntegerVector& idx) {
  CharacterVector src(col);
  int n = idx.size();
  int src_len = src.size();
  CharacterVector out(n);

  for (int i = 0; i < n; i++) {
    int ix = idx[i];
    if (ix == NA_INTEGER || ix < 1 || ix > src_len) {
      out[i] = NA_STRING;
    } else {
      out[i] = src[ix - 1];
    }
  }

  return out;
}

inline SEXP fill_by_index_int(SEXP col, const IntegerVector& idx) {
  IntegerVector src(col);
  int n = idx.size();
  int src_len = src.size();
  IntegerVector out(n);

  for (int i = 0; i < n; i++) {
    int ix = idx[i];
    if (ix == NA_INTEGER || ix < 1 || ix > src_len) {
      out[i] = NA_INTEGER;
    } else {
      out[i] = src[ix - 1];
    }
  }

  if (is_factor(col)) {
    out.attr("levels") = Rf_getAttrib(col, R_LevelsSymbol);
    out.attr("class") = Rf_getAttrib(col, R_ClassSymbol);
  }

  return out;
}

inline SEXP fill_by_index_real(SEXP col, const IntegerVector& idx) {
  NumericVector src(col);
  int n = idx.size();
  int src_len = src.size();
  NumericVector out(n);

  for (int i = 0; i < n; i++) {
    int ix = idx[i];
    if (ix == NA_INTEGER || ix < 1 || ix > src_len) {
      out[i] = NA_REAL;
    } else {
      out[i] = src[ix - 1];
    }
  }

  return out;
}

inline SEXP fill_by_index_lgl(SEXP col, const IntegerVector& idx) {
  LogicalVector src(col);
  int n = idx.size();
  int src_len = src.size();
  LogicalVector out(n);

  for (int i = 0; i < n; i++) {
    int ix = idx[i];
    if (ix == NA_INTEGER || ix < 1 || ix > src_len) {
      out[i] = NA_LOGICAL;
    } else {
      out[i] = src[ix - 1];
    }
  }

  return out;
}

inline CharacterVector make_names(const CharacterVector& x_names,
                                  const CharacterVector& y_names,
                                  const CharacterVector& overlap) {
  int nx = x_names.size();
  int ny = y_names.size();
  CharacterVector out(nx + ny);

  std::unordered_set<std::string> overlap_set;
  for (int i = 0; i < overlap.size(); i++) {
    overlap_set.insert(Rcpp::as<std::string>(overlap[i]));
  }

  for (int i = 0; i < nx; i++) {
    std::string name = Rcpp::as<std::string>(x_names[i]);
    if (overlap_set.find(name) != overlap_set.end()) {
      out[i] = name + ".x";
    } else {
      out[i] = name;
    }
  }

  for (int i = 0; i < ny; i++) {
    std::string name = Rcpp::as<std::string>(y_names[i]);
    if (overlap_set.find(name) != overlap_set.end()) {
      out[nx + i] = name + ".y";
    } else {
      out[nx + i] = name;
    }
  }

  return out;
}

// [[Rcpp::export(rng = false)]]
SEXP bind_by_rowid_cpp(SEXP x_dt,
                       SEXP y_dt,
                       const IntegerVector& x_idx,
                       const IntegerVector& y_idx,
                       const CharacterVector& overlap) {
  DataFrame x_df(x_dt);
  DataFrame y_df(y_dt);

  int n = x_idx.size();
  int nx_cols = x_df.size();
  int ny_cols = y_df.size();

  List out_list(nx_cols + ny_cols);

  for (int j = 0; j < nx_cols; j++) {
    SEXP col = x_df[j];
    switch(TYPEOF(col)) {
      case INTSXP:
        out_list[j] = fill_by_index_int(col, x_idx);
        break;
      case REALSXP:
        out_list[j] = fill_by_index_real(col, x_idx);
        break;
      case LGLSXP:
        out_list[j] = fill_by_index_lgl(col, x_idx);
        break;
      case STRSXP:
        out_list[j] = fill_by_index_str(col, x_idx);
        break;
      default:
        Rcpp::stop("Unsupported column type in x");
    }
  }

  for (int j = 0; j < ny_cols; j++) {
    SEXP col = y_df[j];
    switch(TYPEOF(col)) {
      case INTSXP:
        out_list[nx_cols + j] = fill_by_index_int(col, y_idx);
        break;
      case REALSXP:
        out_list[nx_cols + j] = fill_by_index_real(col, y_idx);
        break;
      case LGLSXP:
        out_list[nx_cols + j] = fill_by_index_lgl(col, y_idx);
        break;
      case STRSXP:
        out_list[nx_cols + j] = fill_by_index_str(col, y_idx);
        break;
      default:
        Rcpp::stop("Unsupported column type in y");
    }
  }

  CharacterVector x_names = x_df.names();
  CharacterVector y_names = y_df.names();
  CharacterVector out_names = make_names(x_names, y_names, overlap);

  out_list.attr("names") = out_names;
  out_list.attr("class") = CharacterVector::create("data.table", "data.frame");
  out_list.attr("row.names") = IntegerVector::create(NA_INTEGER, -n);

  return out_list;
}
