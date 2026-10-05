/* rusage -- run a child and report wall/user/sys/peak-RSS on fd 3.
 *
 * Usage: rusage PROGRAM [ARG...]
 *
 * The child inherits stdin/stdout/stderr unchanged.  One machine-readable
 * line is written to file descriptor 3 (so it does not mix with the
 * program's own stderr):
 *
 *   wall=1.234567 user=1.200000 sys=0.030000 maxrss_kib=31140 exit=0
 *
 * maxrss is the child's peak resident set size as reported by
 * wait4()/RUSAGE_CHILDREN, i.e. the same quantity as GNU time -v's
 * "Maximum resident set size".  No external dependency (no GNU time,
 * no Python).
 */
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: rusage PROGRAM [ARG...]\n");
        return 2;
    }
    struct timespec t0;
    clock_gettime(CLOCK_MONOTONIC, &t0);
    pid_t pid = fork();
    if (pid < 0) {
        perror("fork");
        return 2;
    }
    if (pid == 0) {
        execvp(argv[1], &argv[1]);
        perror("exec");
        _exit(127);
    }
    int status = 0;
    struct rusage usage;
    if (wait4(pid, &status, 0, &usage) < 0) {
        perror("wait4");
        return 2;
    }
    struct timespec t1;
    clock_gettime(CLOCK_MONOTONIC, &t1);
    double wall = (t1.tv_sec - t0.tv_sec) + (t1.tv_nsec - t0.tv_nsec) / 1e9;
    double user = usage.ru_utime.tv_sec + usage.ru_utime.tv_usec / 1e6;
    double sys = usage.ru_stime.tv_sec + usage.ru_stime.tv_usec / 1e6;
    int exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : 128;
    dprintf(3,
            "wall=%.6f user=%.6f sys=%.6f maxrss_kib=%ld exit=%d\n",
            wall, user, sys, usage.ru_maxrss, exit_code);
    if (WIFEXITED(status))
        return WEXITSTATUS(status);
    if (WIFSIGNALED(status))
        return 128 + WTERMSIG(status);
    return 2;
}
