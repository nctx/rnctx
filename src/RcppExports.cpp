// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;


RcppExport SEXP _rcpp_module_boot_CtxUtils();
RcppExport SEXP _rcpp_module_boot_GraphDirected();
RcppExport SEXP _rcpp_module_boot_GraphUndirected();

static const R_CallMethodDef CallEntries[] = {
    {"_rcpp_module_boot_CtxUtils", (DL_FUNC) &_rcpp_module_boot_CtxUtils, 0},
    {"_rcpp_module_boot_GraphDirected", (DL_FUNC) &_rcpp_module_boot_GraphDirected, 0},
    {"_rcpp_module_boot_GraphUndirected", (DL_FUNC) &_rcpp_module_boot_GraphUndirected, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_nctx(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
