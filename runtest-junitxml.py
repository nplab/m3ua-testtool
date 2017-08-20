#!/usr/bin/python3

# Alternative executor for m3ua tests suite by Michael Tuexen's nplab
# (C) 2017 by Harald Welte <laforge@gnumonks.org>

from optparse import OptionParser
import os, sys, signal, time
from subprocess import Popen, PIPE
from junit_xml import TestSuite, TestCase

PROTO='m3ua'
GUILE='/usr/bin/guile'
COMMAND_SKEL="""
    (load-from-path "%s/.guile")
    (let ((test-name "%s"))
      (if (defined? (string->symbol test-name))
          (exit ((eval-string test-name)
                 tester-addr tester-port sut-addr sut-port))
          (exit 254)))
          """;

TEST='%s-test' % PROTO

def status2tc(testcase, exitstatus, time_passed=0, stdout=None, stderr=None):
    tc = TestCase(testcase, TEST, time_passed, stdout, stderr)
    if exitstatus == 0:
        return tc
    elif exitstatus == 1:
        tc.add_failure_info('FAILED')
    elif exitstatus == 2:
        tc.add_error_info('UNKNOWN')
    elif exitstatus == 252:
        tc.add_error_info('TIMEOUT')
    elif exitstatus == 253:
        tc.add_skipped_info('NON-APPLICABLE')
    elif exitstatus == 254:
        tc.add_error_info('NON-EXISTENT')
    elif exitstatus == 255:
        tc.add_error_info("COULDN'T START GUILE")
    else:
        tc.add_error_info("BUG")
    return tc

def signal_handler(signum, frame):
    raise IOError("Timeout!")

def start_testcase(directory, testcase, timeout=0):
    cmd = COMMAND_SKEL % (directory, testcase)
    signal.signal(signal.SIGALRM, signal_handler)
    signal.alarm(timeout)
    before = time.time()
    my_env = os.environ
    my_env["GUILE_WARN_DEPRECATED"] = "no"
    my_env["GUILE_AUTO_COMPILE"] = "0"
    p = Popen([GUILE, '-L', directory, '-c', cmd], env=my_env, stdout=PIPE, stderr=PIPE)
    try:
        (tc_stdout, tc_stderr) = p.communicate()
        returncode = p.returncode
    except IOError:
        tc_stdout = tc_stderr = None
        returncode = 252
    signal.alarm(0)
    after = time.time()
    return status2tc(testcase, returncode, after-before, tc_stdout, tc_stderr)

def parse_options():
    parser = OptionParser(usage="usage: %prog [options] test-case-list.txt")
    parser.add_option('-d', '--directory', dest='directory', metavar='DIR',
                    help='Directory where to look for .guile file',
                    default=os.environ['HOME'])
    parser.add_option('-t', '--timeout', dest='timeout', metavar='TOUT',
                    help='Timeout for individual test case in sconds',
                    default=60, type='int')
    parser.add_option('-s', '--sleep', dest='sleep', metavar='SLEEP',
                    help='Sleep for given amount of time in between tests',
                    default=0, type='float')

    (options, args) = parser.parse_args()

    if len(args) < 1:
        parser.error('You must specify a test list file name')

    return options, args



(options, args) = parse_options()

with open(args[0]) as f:
    cases = f.read().splitlines()

tcs = []

for c in cases:
    res = start_testcase(options.directory, c, timeout=options.timeout)
    tcs.append(res)
    time.sleep(options.sleep)

ts = TestSuite(TEST, tcs)
print((TestSuite.to_xml_string([ts])))
