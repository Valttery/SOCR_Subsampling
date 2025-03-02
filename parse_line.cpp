#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector parse_line_cpp(std::string line, std::string sep, std::string quote) {
    bool use_quote = !quote.empty();
    char quote_char = use_quote ? quote[0] : '\0';

    if (use_quote && quote.size() != 1) {
        Rf_error("Quote must be a single character if provided.");
    }
    if (sep.size() != 1) {
        Rf_error("Separator must be a single character.");
    }
    char delimiter = sep[0];

    std::vector<std::string> fields;
    fields.reserve(10);

    size_t line_len = line.size();
    size_t pos = 0;
    bool in_quotes = false;
    std::string current_field;
    current_field.reserve(64);

    while (pos <= line_len) {
        bool end_of_line = (pos == line_len);
        char c = end_of_line ? '\0' : line[pos];

        if (in_quotes) {
            if (end_of_line) {
                in_quotes = false;
                fields.push_back(current_field);
                current_field.clear();
                break;
            } else if (c == quote_char) {
                if (pos + 1 < line_len && line[pos + 1] == quote_char) {
                    current_field.push_back(quote_char);
                    pos += 2;
                    continue;
                } else {
                    in_quotes = false;
                    pos++;
                    continue;
                }
            } else {
                current_field.push_back(c);
                pos++;
            }
        } else {
            if (end_of_line || c == delimiter) {
                fields.push_back(current_field);
                current_field.clear();
                pos++;
            } else if (use_quote && c == quote_char) {
                in_quotes = true;
                pos++;
            } else {
                current_field.push_back(c);
                pos++;
            }
        }
    }

    if (!current_field.empty() || line.back() == delimiter) {
        fields.push_back(current_field);
    }

    return wrap(fields);
}
