/*-
 * Copyright (c) 2009 Michael Tuexen tuexen@fh-muenster.de
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * $Id: runm3uatest.c,v 1.8 2012/08/25 23:41:55 tuexen Exp $
 */

#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>

#define TIMEOUT        0
#define COMMAND_LENGTH 2048

#define RED(string) isatty(fileno(stdout))?"\033[31m"string"\033[0m":string
#define GREEN(string) isatty(fileno(stdout))?"\033[32m"string"\033[0m":string
#define YELLOW(string) isatty(fileno(stdout))?"\033[33m"string"\033[0m":string
#define BLUE(string) isatty(fileno(stdout))?"\033[34m"string"\033[0m":string

char command_skel[] =
"(load-from-path \"%s/.guile\")"
"(let ((test-name \"%s\"))"
"  (if (defined? (string->symbol test-name))"
"      (exit ((eval-string test-name)"
"             tester-addr tester-port sut-addr sut-port))"
"      (exit 254)))";

char usage[] =
"Usage: runm3uatest [options] testname\n"
"Options:\n"
"        -h       display this help\n"
"        -t time  maximum runtime in seconds (default: no limit)\n";

pid_t pid;

void
handler(int n) {
	kill(pid, SIGKILL);
}

void
print_usage() {
	fprintf(stderr, "%s", usage);
}
int 
main(int argc, char *argv[]) {
	unsigned int timeout;
	int status, c;
	char command[COMMAND_LENGTH];
	char *dir = getenv("HOME");

	timeout = TIMEOUT;

	while ((c = getopt(argc, argv, "t:d:")) != -1) {
		switch(c) {
			case 'h':
				print_usage();
				return (0);
				break;
			case 't':
				timeout = (unsigned int)atoi(optarg);
				break;
			case 'd':
				dir = optarg;
				break;
			default:
				print_usage();
				return (1);
		}
	}

	if (optind == argc - 1) {
		snprintf(command, COMMAND_LENGTH, command_skel, dir, argv[optind]);
	} else {
		print_usage();
		return (1);
	}

	if ((pid = fork()) == 0) {
#if defined(__APPLE__) || defined(__FreeBSD__)
		execlp("/usr/local/bin/guile", "guile", "-L", dir, "-c", command, NULL);
#else
		execlp("/usr/bin/guile", "guile", "-L", dir, "-c", command, NULL);
#endif
		return (255);
	}
	printf("Test %-40.40s ", argv[optind]);
	fflush(stdout);
	if (timeout > 0) {
		signal(SIGALRM, handler);
		alarm(timeout);
	}

	if (wait(&status) == -1) {
		fprintf(stderr, "%s\n", "Couldn't start guile.");
		return (1);
	}
	if (WIFSIGNALED(status)) {
		printf("%-29.29s\n", YELLOW("TIMEOUT"));
	} else {
		switch (WEXITSTATUS(status)) {
			case 0:
				printf("%-29.29s\n", GREEN("PASSED"));
				break;
			case 1:
				printf("%-29.29s\n", RED("FAILED"));
				break;
			case 2:
				printf("%-29.29s\n", YELLOW("UNKNOWN"));
				break;
			case 253:
				printf("%-29.29s\n", BLUE("NON-APPLICABLE"));
				break;
			case 254:
				printf("%-29.29s\n", YELLOW("NON-EXISTENT"));
				break;
			case 255:
				printf("%-29.29s\n", YELLOW("COULDN'T START GUILE"));
				break;
			default:
				printf("%-29.29s\n", YELLOW("BUG"));
				break;
		}
	}
	return (0);
}
