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



SEXP subset_column(SEXP col, const IntegerVector& idx) {
  const int n = idx.size();
  const R_xlen_t src_len = XLENGTH(col);
  const int* p_idx = INTEGER(idx);

  switch (TYPEOF(col)) {
    case INTSXP: {
      IntegerVector out(n);
      int* p_src = INTEGER(col);
      int* p_out = INTEGER(out);
      for (int i = 0; i < n; i++) {
        const int ix = p_idx[i];
        p_out[i] = (ix == NA_INTEGER || ix < 1 || ix > src_len) ? NA_INTEGER : p_src[ix - 1];
      }
      copy_vector_attributes(col, out);
      return out;
    }
    case REALSXP: {
      NumericVector out(n);
      double* p_src = REAL(col);
      double* p_out = REAL(out);
      for (int i = 0; i < n; i++) {
        const int ix = p_idx[i];
        p_out[i] = (ix == NA_INTEGER || ix < 1 || ix > src_len) ? NA_REAL : p_src[ix - 1];
      }
      copy_vector_attributes(col, out);
      return out;
    }
    case LGLSXP: {
      LogicalVector out(n);
      int* p_src = LOGICAL(col);
      int* p_out = LOGICAL(out);
      for (int i = 0; i < n; i++) {
        const int ix = p_idx[i];
        p_out[i] = (ix == NA_INTEGER || ix < 1 || ix > src_len) ? NA_LOGICAL : p_src[ix - 1];
      }
      copy_vector_attributes(col, out);
      return out;
    }
    case STRSXP: {
      SEXP out = PROTECT(Rf_allocVector(STRSXP, n));
      for (int i = 0; i < n; i++) {
        const int ix = p_idx[i];
        if (ix == NA_INTEGER || ix < 1 || ix > src_len) {
          SET_STRING_ELT(out, i, NA_STRING);
        } else {
          SET_STRING_ELT(out, i, STRING_ELT(col, ix - 1));
        }
      }
      copy_vector_attributes(col, out);
      UNPROTECT(1);
      return out;
    }
    case VECSXP: {
      SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
      for (int i = 0; i < n; i++) {
        const int ix = p_idx[i];
        if (ix == NA_INTEGER || ix < 1 || ix > src_len) {
          SET_VECTOR_ELT(out, i, R_NilValue);
        } else {
          SET_VECTOR_ELT(out, i, VECTOR_ELT(col, ix - 1));
        }
      }
      copy_vector_attributes(col, out);
      UNPROTECT(1);
      return out;
    }
    case CPLXSXP: {
      ComplexVector out(n);
      Rcomplex* p_src = COMPLEX(col);
      Rcomplex* p_out = COMPLEX(out);
      for (int i = 0; i < n; i++) {
        const int ix = p_idx[i];
        if (ix == NA_INTEGER || ix < 1 || ix > src_len) {
          p_out[i].r = NA_REAL;
          p_out[i].i = NA_REAL;
        } else {
          p_out[i] = p_src[ix - 1];
        }
      }
      copy_vector_attributes(col, out);
      return out;
    }
    case RAWSXP: {
      RawVector out(n);
      Rbyte* p_src = RAW(col);
      Rbyte* p_out = RAW(out);
      for (int i = 0; i < n; i++) {
        const int ix = p_idx[i];
        p_out[i] = (ix == NA_INTEGER || ix < 1 || ix > src_len) ? 0 : p_src[ix - 1];
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

  CharacterVector base_names(nx_cols + ny_cols);
  CharacterVector x_names = x_df.names();
  CharacterVector y_names = y_df.names();
  for (int j = 0; j < nx_cols; j++) {
    base_names[j] = x_names[j];
  }
  for (int j = 0; j < ny_cols; j++) {
    base_names[nx_cols + j] = y_names[j];
  }
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
