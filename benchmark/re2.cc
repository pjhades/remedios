#include <re2/re2.h>

extern "C" {
    bool re2_full_match(const char *text, const char *pattern) {
        return re2::RE2::FullMatch(text, pattern);
    }

    bool re2_partial_match(const char *text, const char *pattern) {
        return re2::RE2::FullMatch(text, pattern);
    }
}
