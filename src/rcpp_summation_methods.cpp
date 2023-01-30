

#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix rcpp_sum_of_minima_triangle(int n,
                                          NumericVector index,
                                          NumericMatrix inner_terms) {

  NumericMatrix mat(n,n);
  int start_row = 0;
  int end_row = 0;

  for (int i = 0; i < n; i++) {

    int start_col = 0;
    int end_col = 0;

    for (int tmp = start_row + 1; tmp < index.length(); tmp++) {
      if (index(tmp - 1) != index(tmp)) {
        end_row = tmp - 1;
        break;
      }
    }

    for (int j = 0; j < i; j++) {

      for (int tmp = start_col + 1; tmp < index.length(); tmp++) {
        if (index(tmp - 1) != index(tmp)) {
          end_col = tmp - 1;
          break;
        }
      }

      NumericMatrix cur = inner_terms(Range(start_row, end_row),
                                      Range(start_col, end_col));
      for (int p = 0; p < cur.nrow(); p++) {
        mat(i,j) += min(cur( p , _ ));
      }
      for (int p = 0; p < cur.ncol(); p++) {
        mat(i,j) += min(cur( _ , p ));
      }

      mat(i,j) = mat(i,j) / 2;

      start_col = end_col + 1;
      end_col = start_col;
    }

    start_row = end_row + 1;
    end_row = start_row;
  }
  return mat;
}


// [[Rcpp::export]]
NumericMatrix rcpp_sum_of_minima_full(int n,
                                      int m,
                                      NumericVector row_index,
                                      NumericVector col_index,
                                      NumericMatrix inner_terms) {

  NumericMatrix mat(n,m);
  int start_row = 0;
  int end_row = 0;

  for (int i = 0; i < n; i++) {

    int start_col = 0;
    int end_col = 0;

    for (int tmp = start_row + 1; tmp < row_index.length(); tmp++) {
      if (row_index(tmp - 1) != row_index(tmp)) {
        end_row = tmp - 1;
        break;
      }
    }

    for (int j = 0; j < m; j++) {

      for (int tmp = start_col + 1; tmp < col_index.length(); tmp++) {
        if (col_index(tmp - 1) != col_index(tmp)) {
          end_col = tmp - 1;
          break;
        }
      }

      NumericMatrix cur = inner_terms(Range(start_row, end_row),
                                      Range(start_col, end_col));
      for (int p = 0; p < cur.nrow(); p++) {
        mat(i,j) += min(cur( p , _ ));
      }
      for (int p = 0; p < cur.ncol(); p++) {
        mat(i,j) += min(cur( _ , p ));
      }

      mat(i,j) = mat(i,j) / 2;

      start_col = end_col + 1;
      end_col = start_col;
    }

    start_row = end_row + 1;
    end_row = start_row;
  }
  return mat;
}


// [[Rcpp::export]]
NumericMatrix rcpp_double_sum_triangle(int n,
                                       NumericVector index,
                                       NumericMatrix inner_terms) {

  NumericMatrix mat(n,n);
  int start_row = 0;
  int end_row = 0;

  for (int i = 0; i < n; i++) {

    int start_col = 0;
    int end_col = 0;

    for (int tmp = start_row + 1; tmp < index.length(); tmp++) {
      if (index(tmp - 1) != index(tmp)) {
        end_row = tmp - 1;
        break;
      }
    }

    for (int j = 0; j < i; j++) {

      for (int tmp = start_col + 1; tmp < index.length(); tmp++) {
        if (index(tmp - 1) != index(tmp)) {
          end_col = tmp - 1;
          break;
        }
      }

      NumericMatrix cur = inner_terms(Range(start_row, end_row),
                                      Range(start_col, end_col));
      for (int m = 0; m < cur.size(); m++) {
        mat(i,j) += cur[m];
      }

      start_col = end_col + 1;
      end_col = start_col;
    }

    start_row = end_row + 1;
    end_row = start_row;
  }
  return mat;
}



// [[Rcpp::export]]
NumericMatrix rcpp_double_sum_full(int n,
                                   int m,
                                   NumericVector row_index,
                                   NumericVector col_index,
                                   NumericMatrix inner_terms) {

  NumericMatrix mat(n,m);
  int start_row = 0;
  int end_row = 0;

  for (int i = 0; i < n; i++) {

    int start_col = 0;
    int end_col = 0;

    for (int tmp = start_row + 1; tmp < row_index.length(); tmp++) {
      if (row_index(tmp - 1) != row_index(tmp)) {
        end_row = tmp - 1;
        break;
      }
    }

    for (int j = 0; j < m; j++) {

      for (int tmp = start_col + 1; tmp < col_index.length(); tmp++) {
        if (col_index(tmp - 1) != col_index(tmp)) {
          end_col = tmp - 1;
          break;
        }
      }

      NumericMatrix cur = inner_terms(Range(start_row, end_row),
                                      Range(start_col, end_col));
      for (int m = 0; m < cur.size(); m++) {
        mat(i,j) += cur[m];
      }

      start_col = end_col + 1;
      end_col = start_col;
    }

    start_row = end_row + 1;
    end_row = start_row;
  }
  return mat;
}


