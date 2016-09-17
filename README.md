# M3UA Testtool and Testsuite
A test tool for M3UA as specified in [RFC 3332](https://tools.ietf.org/html/rfc3332),
which is obsoleted by [RFC 4666](https://tools.ietf.org/html/rfc4666), and the ETSI specification
[ETSI TS 102 142](http://www.etsi.org/deliver/etsi_ts/102100_102199/102142/01.01.01_60/ts_102142v010101p.pdf).
The tests are based on the ETSI test specification
[ETSI TS 102 381](http://www.etsi.org/deliver/etsi_ts/102300_102399/102381/01.01.01_60/ts_102381v010101p.pdf).

This tool uses [guile](https://www.gnu.org/software/guile/) and its extension 
[guile-sctp](https://github.com/nplab/guile-sctp) for adding SCTP support.
Please see [README](https://github.com/nplab/guile-sctp/blob/master/README.md#installation) for installation instructions.

## Installation
For downloading the tool and the tests run
```
git clone https://github.com/nplab/m3ua-testtool.git
cd m3ua-testtool
```
Then compile the `runm3uatest` using
```
cc -o runm3uatest runm3uatest.c
```
and install the binary. For example, you can issue on Linux
```
sudo cp runm3uatest /usr/bin
```
or do the following under FreeBSD
```
sudo cp runm3uatest /usr/local/bin
```

Finally change the line in `dotguile`
```
(define dir "/Users/tuexen/Documents/m3ua-testtool/")
```
to reflect the location of the `m3ua-testtool` directory and run
```
cp dotguile ~/.guile
```

## Configuration
The test tool is configured by editing the file `m3ua-param-testtool.scm`. You can change
the IP-address and SCTP port numbers used by the test tool and the system under test by
modifying the entries:
```
(define sut-addr "127.0.0.1")
(define sut-port   0)
```
and
```
(define tester-addr "127.0.0.1")
(define tester-port  m3ua-port)
```
Further down, you can also define point codes, routing contexts and even parameters sent in the M3UA handshake.

## Usage
You can run specfic tests using the `runm3uatest`. For example, to run the `m3ua-asp-aspsm-v-002` test, execute
```
runm3uatest m3ua-asp-aspsm-v-002
```
You can also run a selection of ASP tests by using the `tcsh` script
```
m3ua-testtool/run-some-asp-tests
```
or a selection of SGP tests by running the `tcsh` script
```
m3ua-testtool/run-some-sgp-tests
```
Please note that you must verify the verdict by double checking the messages exchanges. This can be done
by using a protocol analyzer like [Wireshark](https://www.wireshark.org).
