# M3UA Testtool
A test tool for M3UA as specified in [RFC 3332](https://tools.ietf.org/html/rfc3332),
which is obsoleted by [RFC 4666](https://tools.ietf.org/html/rfc4666), and the ETSI specification
[ETSI TS 102 142](http://www.etsi.org/deliver/etsi_ts/102100_102199/102142/01.01.01_60/ts_102142v010101p.pdf).
The tests are based on the ETSI test specification
[ETSI TS 102 381](http://www.etsi.org/deliver/etsi_ts/102300_102399/102381/01.01.01_60/ts_102381v010101p.pdf).

## Requirements
This tool uses [guile](https://www.gnu.org/software/guile/) and its extension [guile-sctp](https://github.com/nplab/guile-sctp) for SCTP.

## Supported Platforms
It runs on:
* FreeBSD
* Linux, using the `libsctp-dev`package.
* Mac OS X, using the [SCTP-NKE](https://github.com/sctplab/SCTP_NKE_ElCapitan) for adding kernel SCTP support.
* Solaris.
