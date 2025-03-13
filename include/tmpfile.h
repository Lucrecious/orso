#ifndef TMPFILE_H_
#define TMPFILE_H_

int get_tmpfile(char *filename_template);

#endif

#ifdef TMPFILE_IMPLEMENTATION
#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
#define PATH_MAX _MAX_PATH
#else
#include <unistd.h>
#include <limits.h>
#endif

int get_tmpfile(char *filename_template) {
#ifdef _WIN32
    if (_mktemp_s(filename_template, strlen(filename_template) + 1) != 0) {
        errno = EINVAL;
        return -1;
    }
    int fd = _open(filename_template, _O_CREAT | _O_EXCL | _O_RDWR, _S_IREAD | _S_IWRITE);
    if (fd == -1) {
        perror("Failed to create temporary file");
    }
    return fd;
#else
    return mkstemp(filename_template);
#endif
}
#endif