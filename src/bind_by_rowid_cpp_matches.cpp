#include <Rcpp.h>
#include <unordered_set>
#include <string>

using namespace Rcpp;

namespace {

inline void copy_vector_attributes(SEXP from, SEXP to) {
  Rf_copyMostAttrib(from, to);
  Rf_setAttrib(to, R_NamesSymbol, R_NilValue);
  Rf_setAttrib(to, R_DimSymbol, R_NilValue);
  Rf_setAttrib(to, R_DimNamesSymbol, R_NilValue);
  Rf_setAttrib(to, R_RowNamesSymbol, R_NilValue);
}

inline CharacterVector make_names(const CharacterVector& x_names,
                                  const CharacterVector& y_names,
                                  const CharacterVector& overlap) {
  const int nx = x_names.size();
  const int ny = y_names.size();
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

SEXP subset_column(SEXP col, const IntegerVector& idx) {
  const int n = idx.size();
  const R_xlen_t src_len = XLENGTH(col);

  switch (TYPEOF(col)) {
    case INTSXP: {
      IntegerVector src(col);
      IntegerVector out(n);
      for (int i = 0; i < n; i++) {
        const int ix = idx[i];
        out[i] = (ix == NA_INTEGER || ix < 1 || ix > src_len) ? NA_INTEGER : src[ix - 1];
      }
      copy_vector_attributes(col, out);
      return out;
    }
    case REALSXP: {
      NumericVector src(col);
      NumericVector out(n);
      for (int i = 0; i < n; i++) {
        const int ix = idx[i];
        out[i] = (ix == NA_INTEGER || ix < 1 || ix > src_len) ? NA_REAL : src[ix - 1];
      }
      copy_vector_attributes(col, out);
      return out;
    }
    case LGLSXP: {
      LogicalVector src(col);
      LogicalVector out(n);
      for (int i = 0; i < n; i++) {
        const int ix = idx[i];
        out[i] = (ix == NA_INTEGER || ix < 1 || ix > src_len) ? NA_LOGICAL : src[ix - 1];
      }
      copy_vector_attributes(col, out);
      return out;
    }
    case STRSXP: {
      CharacterVector src(col);
      CharacterVector out(n);
      for (int i = 0; i < n; i++) {
        const int ix = idx[i];
        out[i] = (ix == NA_INTEGER || ix < 1 || ix > src_len) ? NA_STRING : src[ix - 1];
      }
      copy_vector_attributes(col, out);
      return out;
    }
    case VECSXP: {
      List src(col);
      List out(n);
      for (int i = 0; i < n; i++) {
        const int ix = idx[i];
        out[i] = (ix == NA_INTEGER || ix < 1 || ix > src_len) ? R_NilValue : src[ix - 1];
      }
      copy_vector_attributes(col, out);
      return out;
    }
    default:
      Rcpp::stop("Unsupported column type in bind_by_rowid_cpp_matches");
  }
}

void append_extra_columns(List& out_list,
                          CharacterVector& full_names,
                          const DataFrame& matches,
                          int start_pos) {
  CharacterVector matches_names = matches.names();
  int out_pos = start_pos;

  for (int i = 0; i < matches_names.size(); i++) {
    String name = matches_names[i];
    if (name != "x" && name != "y" && name != "i") {
      out_list[out_pos] = matches[i];
      full_names[out_pos] = name;
      out_pos++;
    }
  }
}

} // namespace

// [[Rcpp::export(rng = false)]]
SEXP bind_by_rowid_cpp_matches(SEXP x_dt,
                               SEXP y_dt,
                               SEXP matches_dt,
                               const CharacterVector& overlap) {
  DataFrame x_df(x_dt);
  DataFrame y_df(y_dt);
  DataFrame matches(matches_dt);

  IntegerVector x_idx = matches["x"];
  IntegerVector y_idx = matches["y"];

  const int n = x_idx.size();
  const int nx_cols = x_df.size();
  const int ny_cols = y_df.size();

  CharacterVector matches_names = matches.names();
  int extra_count = 0;
  for (int i = 0; i < matches_names.size(); i++) {
    String name = matches_names[i];
    if (name != "x" && name != "y" && name != "i") {
      extra_count++;
    }
  }

  List out_list(nx_cols + ny_cols + extra_count);

  for (int j = 0; j < nx_cols; j++) {
    out_list[j] = subset_column(x_df[j], x_idx);
  }

  for (int j = 0; j < ny_cols; j++) {
    out_list[nx_cols + j] = subset_column(y_df[j], y_idx);
  }

  CharacterVector base_names = make_names(x_df.names(), y_df.names(), overlap);
  CharacterVector full_names(nx_cols + ny_cols + extra_count);

  for (int i = 0; i < base_names.size(); i++) {
    full_names[i] = base_names[i];
  }

  append_extra_columns(out_list, full_names, matches, nx_cols + ny_cols);

  out_list.attr("names") = full_names;
  out_list.attr("class") = CharacterVector::create("data.table", "data.frame");
  out_list.attr("row.names") = IntegerVector::create(NA_INTEGER, -n);

  return out_list;
}
