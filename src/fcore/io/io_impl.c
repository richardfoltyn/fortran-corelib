

#include <stdio.h>
#include <sys/stat.h>


int fcore_c_is_dir (char *name) {

    struct stat sb;
    int is_dir = 0;

    if (stat(name, &sb) == 0 && S_ISDIR(sb.st_mode)) {
        is_dir = 1;
    }

    return is_dir;
}
