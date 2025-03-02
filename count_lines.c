#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rconn.h>
#include <stdio.h>

// [[register]]
SEXP count_lines(SEXP r_file_path) {
    const char *file_path = CHAR(STRING_ELT(r_file_path, 0));
    FILE *fp = fopen(file_path, "rb");
    if (!fp) {
        Rf_error("Could not open file: %s", file_path);
    }

    char buf[65536];
    size_t nread;
    long long line_count = 0;
    while ((nread = fread(buf, 1, sizeof(buf), fp)) > 0) {
        for (size_t i = 0; i < nread; i++) {
            if (buf[i] == '\n') line_count++;
        }
    }

    fclose(fp);
    return ScalarReal((double)line_count);
}