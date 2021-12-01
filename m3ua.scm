;;; 
;;; Copyright (C) 2004, 2005, 2006 M. Tuexen tuexen@fh-muenster.de
;;; 
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or
;;; without modification, are permitted provided that the
;;; following conditions are met:
;;; 1. Redistributions of source code must retain the above
;;;    copyright notice, this list of conditions and the
;;;    following disclaimer.
;;; 2. Redistributions in binary form must reproduce the
;;;    above copyright notice, this list of conditions and
;;;    the following disclaimer in the documentation and/or
;;;    other materials provided with the distribution.
;;; 3. Neither the name of the project nor the names of
;;;    its contributors may be used to endorse or promote
;;;    products derived from this software without specific
;;;    prior written permission.
;;;  
;;; THIS SOFTWARE IS PROVIDED BY THE PROJECT AND CONTRIBUTORS
;;; ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
;;; BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED.  IN NO EVENT SHALL THE PROJECT OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
;;; IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
;;; OF SUCH DAMAGE.

;;; $Id: m3ua.scm,v 1.22 2012/08/28 19:56:13 tuexen Exp $

;;; Version 1.1.10
;;;
;;; History of changes:
;;; 04.12.2004 m3ua-reserved-aspsm-message-type added
;;; 04.12.2004 m3ua-make-correlation-id-parameter added
;;; 04.12.2004 m3ua-make-network-appearance-parameter added
;;; 04.12.2004 m3ua-make-asp-parameter added
;;; 04.12.2004 m3ua-traffic-mode-type-broadcast added
;;; 04.12.2004 m3ua-make-asp-up-message now accepts parameters
;;; 04.12.2004 m3ua-make-asp-inactive-message now accepts parameters
;;; 04.12.2004 m3ua-make-asp-inactive-ack-message now accepts parameters
;;; 04.12.2004 m3ua-make-data-message now accepts parameters
;;; 14.12.2004 m3ua-error-message? added
;;; 18.12.2004 m3ua-make-data-message takes now ni mp and sls
;;; 19.12.2004 m3ua-notify-message? added.
;;; 19.12.2004 m3ua-run-sgp accepts a port.
;;; 19.12.2004 m3ua-data-message? added.
;;; 19.12.2004 m3ua-make-routing-context-parameter takes a list of contexts.
;;; 09.09.2005 m3ua-recv-message handles system errors
;;; 09.09.2005 m3ua-wait-for-message returns also on empty messages
;;; 09.09.2005 Use SCTP_NODELAY for all sockets
;;; 10.09.2005 Do the htonl() conversion of the PPID in the scheme code
;;; 04.10.2005 Fix syntax error in m3ua-make-asp-active-ack-message
;;; 04.10.2005 Handle the case where SCTP_NODELAY is not defined
;;; 09.10.2005 Extend m3ua-run-asp to be able to test the APS tests.
;;; 23.12.2005 Add m3ua-send-beats.
;;; 02.01.2006 Added all missing parameter constructors.
;;; 02.01.2006 Added support for RKM messages.
;;; 18.02.2006 Added support for generating REG_RSP messages and the CIC range parameter
;;; 12.03.2006 m3ua-check-common-header now optionally supports RKM messages.
;;; 13.09.2006 Remove info parameter from m3ua-make-data-message.
;;; 11.03.2007 Catch system-errors in send and recv calls.

(define m3ua-test-result-passed 0)
(define m3ua-test-result-failed 1)
(define m3ua-test-result-unknown 2)
(define m3ua-test-result-not-applicable 253)

;;; This is the IANA registered PPID for M3UA in host byte order
(define m3ua-ppid                        3)

;;; This is the IANA registered port for M3UA
(define m3ua-port                     2905)

;;; Constants for the message classes
(define m3ua-mgmt-message-class          0)
(define m3ua-tfer-message-class          1)
(define m3ua-ssnm-message-class          2)
(define m3ua-aspsm-message-class         3)
(define m3ua-asptm-message-class         4)
(define m3ua-rkm-message-class           9)
(define m3ua-reserved-message-class     99)

;;; Constants for the message types
;;; MGMT messages
(define m3ua-err-message-type            0)
(define m3ua-ntfy-message-type           1)

;;; TFER messages
(define m3ua-data-message-type           1)
(define m3ua-reserved-tfer-message-type  2)

;;; SSNM messages
(define m3ua-duna-message-type           1)
(define m3ua-dava-message-type           2)
(define m3ua-daud-message-type           3)
(define m3ua-scon-message-type           4)
(define m3ua-dupu-message-type           5)
(define m3ua-drst-message-type           6)

;;; ASPSM messages
(define m3ua-aspup-message-type          1)
(define m3ua-aspdn-message-type          2)
(define m3ua-beat-message-type           3)
(define m3ua-aspup-ack-message-type      4)
(define m3ua-aspdn-ack-message-type      5)
(define m3ua-beat-ack-message-type       6)
(define m3ua-reserved-aspsm-message-type 7)

;;;ASPTM messages
(define m3ua-aspac-message-type          1)
(define m3ua-aspia-message-type          2)
(define m3ua-aspac-ack-message-type      3)
(define m3ua-aspia-ack-message-type      4)
(define m3ua-reserved-asptm-message-type 5)

;;; RKM messages
(define m3ua-reg-req-message-type        1)
(define m3ua-reg-rsp-message-type        2)
(define m3ua-dereg-req-message-type      3)
(define m3ua-dereg-rsp-message-type      4)
(define m3ua-reserved-rkm-message-type   5)

;;; Constant for the protocol version
(define m3ua-version                     1)

;;; Constant for reserved
(define m3ua-reserved                    0)

;;;
;;; Creator functions for messages
;;;

(define (m3ua-make-common-header version reserved class type length)
  (append (uint8->bytes version)
	  (uint8->bytes reserved)
	  (uint8->bytes class)
	  (uint8->bytes type)
	  (uint32->bytes length)))

;;;(m3ua-make-common-header 1 2 3 4 5)
;;;(m3ua-make-common-header m3ua-version m3ua-reserved m3ua-tfer-message-class m3ua-data-message-type 16)

(define (m3ua-increment-version l)
  (if (positive? (length l))
      (cons (+ (car l) 1) (cdr l))
      (list)))
;;;(m3ua-increment-version (list 1 2 3))
;;;(m3ua-increment-version (list))

;;;
;;; Creator functions for parameters
;;;

(define m3ua-parameter-header-length 4)
(define m3ua-common-header-length 8)
(define m3ua-data-parameter-header-length 16)

(define (m3ua-number-of-padding-bytes l)
  (remainder (- 4 (remainder l 4)) 4))
;;; (m3ua-number-of-padding-bytes 0)
;;; (m3ua-number-of-padding-bytes 1)
;;; (m3ua-number-of-padding-bytes 2)
;;; (m3ua-number-of-padding-bytes 3)

(define (m3ua-add-padding l)
  (+ l (m3ua-number-of-padding-bytes l)))
;;; (m3ua-add-padding 2)

(define (m3ua-padding data)
  (zero-bytes (m3ua-number-of-padding-bytes (length data))))
;;;(m3ua-padding (list 1 2 3 4 5))

(define (m3ua-make-parameter tag value)
  (append (uint16->bytes tag)
	  (uint16->bytes (+ (length value) m3ua-parameter-header-length))
	  value
	  (m3ua-padding value)))

(define (m3ua-make-random-parameter l)
  (m3ua-make-parameter (random 2^16) (random-bytes l)))
;;;(m3ua-make-random-parameter 10)

(define (m3ua-add-parameter parameter list)
  (cons parameter (remove (lambda(p) (equal? (m3ua-get-parameter-tag p)
					     (m3ua-get-parameter-tag parameter)))
			  list)))
;;;(m3ua-add-parameter (m3ua-make-info-string-parameter "Hello1") (list (m3ua-make-correlation-id-parameter 34)))
;;;(m3ua-add-parameter (m3ua-make-info-string-parameter "Hello1") (list (m3ua-make-correlation-id-parameter 34) (m3ua-make-info-string-parameter "Hello")))

(define (m3ua-make-message class type parameters)
  (append (m3ua-make-common-header m3ua-version
				   m3ua-reserved
				   class
				   type
				   (+ m3ua-common-header-length (apply + (map length parameters))))
	  (apply append parameters)))

(define m3ua-info-string-tag                  #x0004)
(define m3ua-routing-context-tag              #x0006)
(define m3ua-diagnostic-info-tag              #x0007)
(define m3ua-heartbeat-data-tag               #x0009)
(define m3ua-traffic-mode-type-tag            #x000b)
(define m3ua-error-code-tag                   #x000c)
(define m3ua-status-tag                       #x000d)
(define m3ua-asp-identifier-tag               #x0011)
(define m3ua-affected-point-code-tag          #x0012)
(define m3ua-correlation-id-tag               #x0013)

(define m3ua-network-appearance-tag           #x0200)
(define m3ua-user-cause-tag                   #x0204)
(define m3ua-congestion-indications-tag       #x0205)
(define m3ua-concerned-destination-tag        #x0206)
(define m3ua-routing-key-tag                  #x0207)
(define m3ua-registration-result-tag          #x0208)
(define m3ua-deregistration-result-tag        #x0209)
(define m3ua-local-routing-key-identifier-tag #x020a)
(define m3ua-destination-point-code-tag       #x020b)
(define m3ua-service-indicators-tag           #x020c)
(define m3ua-originating-point-code-list-tag  #x020e)
(define m3ua-circuit-range-tag                #x020f)
(define m3ua-protocol-data-tag                #x0210)
(define m3ua-registration-status-tag          #x0212)
(define m3ua-deregistration-status-tag        #x0213)

(define (m3ua-make-info-string-parameter string)
  (m3ua-make-parameter m3ua-info-string-tag (string->bytes string)))
;;; (m3ua-make-info-string-parameter "Hello")

(define (m3ua-make-routing-context-parameter contexts)
  (m3ua-make-parameter m3ua-routing-context-tag (apply append (map uint32->bytes contexts))))
;;; (m3ua-make-routing-context-parameter (list 1024))
;;; (m3ua-make-routing-context-parameter (list))
;;; (m3ua-make-routing-context-parameter (list 1024 4 5 6))

(define (m3ua-make-diagnostic-info-parameter info)
  (m3ua-make-parameter m3ua-diagnostic-info-tag info))
;;; (m3ua-make-diagnostic-info-parameter (list 1 2 3 4 5))

(define (m3ua-make-heartbeat-data-parameter data)
  (m3ua-make-parameter m3ua-heartbeat-data-tag data))
;;; (m3ua-make-heartbeat-data-parameter (string->bytes "M3UA rocks"))

(define m3ua-traffic-mode-type-override  1)
(define m3ua-traffic-mode-type-loadshare 2)
(define m3ua-traffic-mode-type-broadcast 3)
(define m3ua-traffic-mode-type-invalid   4)

(define (m3ua-make-traffic-mode-type-parameter mode)
  (m3ua-make-parameter m3ua-traffic-mode-type-tag (uint32->bytes mode)))
;;; (m3ua-make-traffic-mode-type-parameter m3ua-traffic-mode-type-override)

(define m3ua-invalid-version-error-code               #x0001)
(define m3ua-unsupported-message-class-error-code     #x0003)
(define m3ua-unsupported-message-type-error-code      #x0004)
(define m3ua-unsupported-traffic-mode-type-error-code #x0005)
(define m3ua-unexpected-message-error-code            #x0006)
(define m3ua-protocol-error-error-code                #x0007)
(define m3ua-invalid-stream-identifier-error-code     #x0009)
(define m3ua-refused-management-blocking-error-code   #x000d)
(define m3ua-asp-identifier-required-error-code       #x000e)
(define m3ua-invalid-parameter-value-error-code       #x0011)
(define m3ua-parameter-field-error-error-code         #x0012)
(define m3ua-unexpected-parameter-error-code          #x0013)
(define m3ua-destination-status-unknown-error-code    #x0014)
(define m3ua-invalid-network-appearance-error-code    #x0015)
(define m3ua-missing-parameter-error-code             #x0016)
(define m3ua-invalid-routing-context-error-code       #x0019)
(define m3ua-no-configure-as-for-asp-error-code       #x001a)

(define (m3ua-make-error-code-parameter code)
  (m3ua-make-parameter m3ua-error-code-tag (uint32->bytes code)))
;;; (m3ua-make-error-code-parameter m3ua-protocol-error-error-code)

(define (m3ua-get-error-code-from-parameter p)
  (bytes->uint32 (m3ua-get-parameter-value p)))
;;;(m3ua-get-error-code-from-parameter (m3ua-make-error-code-parameter m3ua-protocol-error-error-code))

(define m3ua-as-state-change-status-type 1)
(define m3ua-other-status-type           2)

(define m3ua-as-inactive                 2)
(define m3ua-as-active                   3)
(define m3ua-as-pending                  4)

(define m3ua-insufficient-resources      1)
(define m3ua-alternate-asp-active        2)
(define m3ua-asp-failure                 3)

(define (m3ua-make-status-parameter type info)
  (m3ua-make-parameter m3ua-status-tag
		       (append (uint16->bytes type)
			       (uint16->bytes info))))
;;; (m3ua-make-status-parameter 2 3)

(define (m3ua-get-status-type-from-parameter l)
  (bytes->uint16 (m3ua-get-parameter-value l)))
;;; (m3ua-get-status-type-from-parameter (m3ua-make-status-parameter 2 3))

(define (m3ua-get-status-info-from-parameter l)
  (bytes->uint16  (list-tail (m3ua-get-parameter-value l) 2)))
;;;  (m3ua-get-status-info-from-parameter (m3ua-make-status-parameter 2 3))

(define (m3ua-make-asp-id-parameter aid)
  (m3ua-make-parameter m3ua-asp-identifier-tag (uint32->bytes aid)))
;;; (m3ua-make-asp-id-parameter 1024)

(define (m3ua-make-affected-point-code-parameter mask-pc-pair-list)
  (m3ua-make-parameter m3ua-affected-point-code-tag
		       (apply append (map (lambda (x)
					    (append (uint8->bytes (car x))
						    (uint24->bytes (cadr x))))
					  mask-pc-pair-list))))
;;; (m3ua-make-affected-point-code-parameter (list (list 0 34) (list 255 89)))

(define (m3ua-make-correlation-id-parameter id)
  (m3ua-make-parameter m3ua-correlation-id-tag (uint32->bytes id)))
;;; (m3ua-make-correlation-id-parameter 1024)

(define (m3ua-make-network-appearance-parameter na)
  (m3ua-make-parameter m3ua-network-appearance-tag (uint32->bytes na)))
;;; (m3ua-make-network-appearance-parameter 1024)

(define m3ua-unknown-cause                  0)
(define m3ua-unequipped-remote-user-cause   1)
(define m3ua-inaccessible-remote-user-cause 2)

(define m3ua-mtp-user-sccp                   3)
(define m3ua-mtp-user-tup                    4)
(define m3ua-mtp-user-isup                   5)
(define m3ua-mtp-user-broadband-isup         9)
(define m3ua-mtp-user-satellite-isup        10)
(define m3ua-mtp-user-aal-type-2-signalling 12)
(define m3ua-mtp-user-bicc                  13)
(define m3ua-mtp-user-gcp                   14)

(define (m3ua-make-user-cause-parameter user cause)
  (m3ua-make-parameter m3ua-user-cause-tag (append (uint16->bytes cause)
						   (uint16->bytes user))))
;;; (m3ua-make-user-cause-parameter m3ua-mtp-user-isup m3ua-unknown-cause)

(define m3ua-no-congestion-level 0)
(define m3ua-congestion-level-1  1)
(define m3ua-congestion-level-2  2)
(define m3ua-congestion-level-3  3)

(define (m3ua-make-congestion-indications-parameter level)
  (m3ua-make-parameter m3ua-congestion-indications-tag (append (uint24->bytes 0)
							       (uint8->bytes level))))
;;; (m3ua-make-congestion-indications-parameter m3ua-congestion-level-2)

(define (m3ua-make-concerned-destination-parameter pc)
  (m3ua-make-parameter m3ua-concerned-destination-tag (append (uint8->bytes 0)
							      (uint24->bytes pc))))
;;; (m3ua-make-concerned-destination-parameter 45)

(define (m3ua-make-routing-key-parameter parameterlist)
  (m3ua-make-parameter m3ua-routing-key-tag (apply append parameterlist)))
;;; (m3ua-make-routing-key-parameter (list (m3ua-make-local-routing-key-identifier-parameter 12) (m3ua-make-destination-point-code-parameter 34)))

(define (m3ua-make-registration-result-parameter parameterlist)
  (m3ua-make-parameter m3ua-registration-result-tag (apply append parameterlist)))
;;; (m3ua-make-registration-result-parameter (list (m3ua-make-local-routing-key-identifier-parameter 1234) (m3ua-make-registration-status-parameter m3ua-successfully-registered-reg-status) (m3ua-make-routing-context-parameter (list 12))))

(define (m3ua-make-deregistration-result-parameter parameterlist)
  (m3ua-make-parameter m3ua-deregistration-result-tag (apply append parameterlist)))
;;; (m3ua-make-deregistration-result-parameter (list (m3ua-make-routing-context-parameter (list 12)) (m3ua-make-deregistration-status-parameter m3ua-successfully-deregistered-dereg-status)))

(define (m3ua-make-local-routing-key-identifier-parameter id)
  (m3ua-make-parameter m3ua-local-routing-key-identifier-tag (uint32->bytes id)))
;;; (m3ua-make-local-routing-key-identifier-parameter 234)

(define (m3ua-make-destination-point-code-parameter pc)
  (m3ua-make-parameter m3ua-destination-point-code-tag (append (uint8->bytes 0)
							       (uint24->bytes pc))))
;;; (m3ua-make-destination-point-code-parameter 45)

(define (m3ua-make-circuit-range-parameter pc-cic-triple-list)
  (m3ua-make-parameter m3ua-circuit-range-tag
		       (apply append (map (lambda (x)
					    (append (uint8->bytes 0)
						    (uint24->bytes (car x))
						    (uint16->bytes (cadr x))
						    (uint16->bytes (caddr x))))
					  pc-cic-triple-list))))
;;; (m3ua-make-circuit-range-parameter (list (list 1 2 3) (list 4 5 6)))

(define (m3ua-make-service-indicators-parameter si-list)
  (m3ua-make-parameter m3ua-service-indicators-tag (apply append (map uint8->bytes si-list))))
;;; (m3ua-make-service-indicators-parameter (list 2 4))

(define (m3ua-make-originating-point-code-list-parameter mask-pc-pair-list)
  (m3ua-make-parameter m3ua-originating-point-code-list-tag
		       (apply append (map (lambda (x)
					    (append (uint8->bytes (car x))
						    (uint24->bytes (cadr x))))
					  mask-pc-pair-list))))

;;; (m3ua-make-originating-point-code-list-parameter (list (list 0 34) (list 255 89)))

(define (m3ua-make-data-parameter opc dpc si ni mp sls data)
  (m3ua-make-parameter m3ua-protocol-data-tag
		       (append (uint32->bytes opc)
			       (uint32->bytes dpc)
			       (uint8->bytes si)
			       (uint8->bytes ni)
			       (uint8->bytes mp)
			       (uint8->bytes sls)
			       data)))
;;; (m3ua-make-data-parameter 3 4 3 2 1 3 (list 1 2 3))

(define m3ua-successfully-registered-reg-status                          0)
(define m3ua-error-unknown-reg-status                                    1)
(define m3ua-error-invalid-dpc-reg-status                                2)
(define m3ua-error-invalid-network-appearance-reg-status                 3)
(define m3ua-error-invalid-routing-key-reg-status                        4)         
(define m3ua-error-permission-denied-reg-status                          5)
(define m3ua-error-cannot-support-unique-routing-reg-status              6)
(define m3ua-error-routing-key-not-currently-provisioned-reg-status      7)
(define m3ua-error-insufficient-resources-reg-status                     8)
(define m3ua-error-unsupported-rk-parameter-field-reg-status             9)
(define m3ua-error-unsupported-invalid-traffic-handling-mode-reg-status 10)
(define m3ua-error-routing-key-change-refused-reg-status                11)
(define m3ua-error-routing-key-already-registered-req-status            12)

(define (m3ua-make-registration-status-parameter status)
  (m3ua-make-parameter m3ua-registration-status-tag (uint32->bytes status)))
;;; (m3ua-make-registration-status-parameter 123)

(define m3ua-successfully-deregistered-dereg-status                      0)
(define m3ua-error-unknown-dereg-status                                  1)
(define m3ua-error-invalid-routing-context-dereg-status                  2)
(define m3ua-error-permission-denied-dereg-status                        3)
(define m3ua-error-not-registered-dereg-status                           4)
(define m3ua-error-asp-currently-active-for-routing-context-dereg-status 5)

(define (m3ua-make-deregistration-status-parameter status)
  (m3ua-make-parameter m3ua-deregistration-status-tag (uint32->bytes status)))
;;; (m3ua-make-deregistration-status-parameter 123)


;;;------------------------------------------------------------------
;;; Parameter Predicates
;;;------------------------------------------------------------------

(define (m3ua-error-code-parameter? l)
  (= (m3ua-get-parameter-tag l) m3ua-error-code-tag))

(define (m3ua-status-parameter? l)
  (= (m3ua-get-parameter-tag l) m3ua-status-tag))

(define (m3ua-routing-key-parameter? l)
  (= (m3ua-get-parameter-tag l) m3ua-routing-key-tag))

(define (m3ua-local-routing-key-identifier-parameter? l)
  (= (m3ua-get-parameter-tag l) m3ua-local-routing-key-identifier-tag))

(define (m3ua-routing-context-parameter? l)
  (= (m3ua-get-parameter-tag l) m3ua-routing-context-tag))

(define (m3ua-registration-result-parameter? l)
  (= (m3ua-get-parameter-tag l) m3ua-registration-result-tag))

;;;------------------------------------------------------------------
;;; Message Contructors
;;;------------------------------------------------------------------

(define (m3ua-make-error-message code)
  (m3ua-make-message m3ua-mgmt-message-class
		     m3ua-err-message-type
		     (list (m3ua-make-error-code-parameter code))))
;;; (m3ua-make-error-message m3ua-no-configure-as-for-asp-error-code)

(define (m3ua-make-notify-message type info)
  (m3ua-make-message m3ua-mgmt-message-class
		     m3ua-ntfy-message-type
		     (list (m3ua-make-status-parameter type info))))
;;; (m3ua-make-notify-message m3ua-as-state-change-status-type m3ua-as-inactive)

(define (m3ua-make-beat-message data)
  (m3ua-make-message m3ua-aspsm-message-class 
		     m3ua-beat-message-type 
		     (list (m3ua-make-heartbeat-data-parameter data))))
;;; (m3ua-make-beat-message (string->bytes "M3UA rocks"))

(define (m3ua-make-beat-ack-message data)
  (m3ua-make-message m3ua-aspsm-message-class
		     m3ua-beat-ack-message-type
		     (list (m3ua-make-heartbeat-data-parameter data))))
;;; (m3ua-make-beat-ack-message (string->bytes "M3UA rocks"))

(define (m3ua-make-asp-up-message parameters)
  (m3ua-make-message m3ua-aspsm-message-class
		     m3ua-aspup-message-type
		     (m3ua-add-parameter (m3ua-make-info-string-parameter "M3UA rocks") parameters)))
;;; (m3ua-make-asp-up-message (list))

(define (m3ua-make-asp-down-message)
  (m3ua-make-message m3ua-aspsm-message-class
		     m3ua-aspdn-message-type
		     (list (m3ua-make-info-string-parameter "M3UA rocks"))))
;;; (m3ua-make-asp-down-message)

(define (m3ua-make-asp-up-ack-message)
  (m3ua-make-message m3ua-aspsm-message-class
		     m3ua-aspup-ack-message-type
		     (list (m3ua-make-info-string-parameter "M3UA rocks"))))
;;; (m3ua-make-asp-up-ack-message)

(define (m3ua-make-asp-down-ack-message)
  (m3ua-make-message m3ua-aspsm-message-class
		     m3ua-aspdn-ack-message-type
		     (list (m3ua-make-info-string-parameter "M3UA rocks"))))
;;; (m3ua-make-asp-down-ack-message)

(define (m3ua-make-asp-active-message parameters)
  (m3ua-make-message m3ua-asptm-message-class
		     m3ua-aspac-message-type
		     (m3ua-add-parameter (m3ua-make-info-string-parameter "M3UA rocks") parameters)))
;;; (m3ua-make-asp-active-message (list (m3ua-make-routing-context-parameter (list 3))))

(define (m3ua-make-asp-active-ack-message parameters)
  (m3ua-make-message m3ua-asptm-message-class
		     m3ua-aspac-ack-message-type
		     (m3ua-add-parameter (m3ua-make-info-string-parameter "M3UA rocks") parameters)))
;;; (m3ua-make-asp-active-ack-message (list))

(define (m3ua-make-asp-inactive-message parameters)
  (m3ua-make-message m3ua-asptm-message-class
		     m3ua-aspia-message-type
		     (m3ua-add-parameter (m3ua-make-info-string-parameter "M3UA rocks") parameters)))
;;; (m3ua-make-asp-inactive-message (list))

(define (m3ua-make-asp-inactive-ack-message parameters)
  (m3ua-make-message m3ua-asptm-message-class
		     m3ua-aspia-ack-message-type
		     (m3ua-add-parameter (m3ua-make-info-string-parameter "M3UA rocks") parameters)))
;;; (m3ua-make-asp-inactive-ack-message (list))

(define (m3ua-make-data-message opc dpc si ni mp sls data parameters)
  (m3ua-make-message m3ua-tfer-message-class
		     m3ua-data-message-type
		     (append parameters
			     (list (m3ua-make-data-parameter opc dpc si ni mp sls data)))))
;;; (m3ua-make-data-message 1 2 3 4 5 6 (list 1 2) (list))
;;; FIXME: Make sure that no parameter is duplicated.

(define (m3ua-make-duna-message parameters)
  (m3ua-make-message m3ua-ssnm-message-class
		     m3ua-duna-message-type
		     (m3ua-add-parameter (m3ua-make-info-string-parameter "M3UA rocks") parameters)))
;;; (m3ua-make-duna-message (list))

(define (m3ua-make-dava-message parameters)
  (m3ua-make-message m3ua-ssnm-message-class
		     m3ua-dava-message-type
		     (m3ua-add-parameter (m3ua-make-info-string-parameter "M3UA rocks") parameters)))
;;; (m3ua-make-dava-message (list))

(define (m3ua-make-daud-message parameters)
  (m3ua-make-message m3ua-ssnm-message-class
		     m3ua-daud-message-type
		     (m3ua-add-parameter (m3ua-make-info-string-parameter "M3UA rocks") parameters)))
;;; (m3ua-make-daud-message (list))

(define (m3ua-make-scon-message parameters)
  (m3ua-make-message m3ua-ssnm-message-class
		     m3ua-scon-message-type
		     (m3ua-add-parameter (m3ua-make-info-string-parameter "M3UA rocks") parameters)))
;;; (m3ua-make-scon-message (list))

(define (m3ua-make-dupu-message parameters)
  (m3ua-make-message m3ua-ssnm-message-class
		     m3ua-dupu-message-type
		     (m3ua-add-parameter (m3ua-make-info-string-parameter "M3UA rocks") parameters)))
;;; (m3ua-make-dupu-message (list))

(define (m3ua-make-drst-message parameters)
  (m3ua-make-message m3ua-ssnm-message-class
		     m3ua-drst-message-type
		     (m3ua-add-parameter (m3ua-make-info-string-parameter "M3UA rocks") parameters)))
;;; (m3ua-make-drst-message (list))

(define (m3ua-make-reg-req-message parameters)
  (m3ua-make-message m3ua-rkm-message-class
		     m3ua-reg-req-message-type
		     (m3ua-add-parameter (m3ua-make-info-string-parameter "M3UA rocks") parameters)))

(define (m3ua-make-reg-rsp-message parameters)
  (m3ua-make-message m3ua-rkm-message-class
		     m3ua-reg-rsp-message-type
		     (m3ua-add-parameter (m3ua-make-info-string-parameter "M3UA rocks") parameters)))


(define (m3ua-make-dereg-req-message parameters)
  (m3ua-make-message m3ua-rkm-message-class
		     m3ua-dereg-req-message-type
		     (m3ua-add-parameter (m3ua-make-info-string-parameter "M3UA rocks") parameters)))


(define (m3ua-make-dereg-rsp-message parameters)
  (m3ua-make-message m3ua-rkm-message-class
		     m3ua-dereg-rsp-message-type
		     (m3ua-add-parameter (m3ua-make-info-string-parameter "M3UA rocks") parameters)))

;;;
;;; General accessor functions for messages
;;;

(define (m3ua-get-common-header l)
  (list-head l m3ua-common-header-length))
;;; (m3ua-get-common-header (m3ua-make-asp-up-message (list)))

(define m3ua-version-offset        0)
(define m3ua-reserved-offset       1)
(define m3ua-message-class-offset  2)
(define m3ua-message-type-offset   3)
(define m3ua-message-length-offset 4)

(define (m3ua-get-version l)
  (bytes->uint8 (list-tail l m3ua-version-offset)))

;;;(define hb (m3ua-make-beat-message (string->bytes "M3UA rocks")))
;;;(m3ua-get-version hb)

(define (m3ua-get-reserved l)
  (bytes->uint8 (list-tail l m3ua-reserved-offset)))
;;;(m3ua-get-reserved hb)

(define (m3ua-get-message-class l)
  (bytes->uint8 (list-tail l m3ua-message-class-offset)))
;;;(m3ua-get-message-class hb)

(define (m3ua-get-message-type l)
  (bytes->uint8 (list-tail l m3ua-message-type-offset)))
;;;(m3ua-get-message-type hb)

(define (m3ua-get-message-length l)
  (bytes->uint32 (list-tail l m3ua-message-length-offset)))
;;;(m3ua-get-message-length hb)

(define (m3ua-get-parameters-1 l)
  (if (>= (length l) m3ua-parameter-header-length)
      (let ((parameter-length (m3ua-add-padding (m3ua-get-parameter-length l))))
	(cons (list-head l parameter-length)
	      (m3ua-get-parameters-1 (list-tail l parameter-length))))
      (list)))

(define (m3ua-get-parameters-of-message l)
  (if (>= (length l) m3ua-common-header-length)
      (m3ua-get-parameters-1 (list-tail l m3ua-common-header-length))
      (list)))
;;; (m3ua-get-parameters-of-message (m3ua-make-beat-message (string->bytes "M3UA rocks")))
;;; (m3ua-get-parameters-of-message (list 2 2))

(define m3ua-get-parameters m3ua-get-parameters-of-message)

(define (m3ua-get-parameters-of-parameter l)
  (if (>= (length l) m3ua-common-header-length)
      (m3ua-get-parameters-1 (list-tail l m3ua-parameter-header-length))
      (list)))
;;; (m3ua-get-parameters-of-parameter (m3ua-make-routing-key-parameter (list (m3ua-make-local-routing-key-identifier-parameter 3) (m3ua-make-destination-point-code-parameter 4))))

(define (m3ua-make-registration-result-from-routing-key key status)
  (let ((local-rk-id (bytes->uint32 (list-tail (car (filter m3ua-local-routing-key-identifier-parameter?
							    (m3ua-get-parameters-of-parameter key)))
					       m3ua-parameter-header-length))))
    (if (= status m3ua-successfully-registered-reg-status)
	(let ((routing-contexts (filter m3ua-routing-context-parameter? (m3ua-get-parameters-of-parameter key))))
	  (if (null? routing-contexts)
	      (m3ua-make-registration-result-parameter (list (m3ua-make-local-routing-key-identifier-parameter local-rk-id)
							     (m3ua-make-registration-status-parameter status)
							     (m3ua-make-routing-context-parameter (list tester-rc-valid))))
	      (let ((rc (bytes->uint32 (list-tail routing-contexts m3ua-parameter-header-length))))
	      	(m3ua-make-registration-result-parameter (list (m3ua-make-local-routing-key-identifier-parameter local-rk-id)
							       (m3ua-make-registration-status-parameter status)
							       (m3ua-make-routing-context-parameter (list rc)))))))
	(m3ua-make-registration-result-parameter (list (m3ua-make-local-routing-key-identifier-parameter local-rk-id)
						       (m3ua-make-registration-status-parameter status)
						       (m3ua-make-routing-context-parameter (list 0)))))))

;;;(m3ua-make-registration-result-from-routing-key (m3ua-make-routing-key-parameter (list (m3ua-make-local-routing-key-identifier-parameter 3) (m3ua-make-destination-point-code-parameter 4))) 0)

(define (m3ua-make-reg-rsp-from-reg-req reg-req)
  (let ((routing-keys (filter m3ua-routing-key-parameter? (m3ua-get-parameters-of-message reg-req))))
    (m3ua-make-reg-rsp-message
     (cons (m3ua-make-registration-result-from-routing-key (car routing-keys) m3ua-successfully-registered-reg-status)
	   (map (lambda (key) (m3ua-make-registration-result-from-routing-key key m3ua-error-insufficient-resources-reg-status))
		(cdr routing-keys))))))

;;;(m3ua-make-reg-rsp-from-reg-req (m3ua-make-reg-req-message (list (m3ua-make-routing-key-parameter (list (m3ua-make-local-routing-key-identifier-parameter 3) (m3ua-make-destination-point-code-parameter 4))))))


(define (m3ua-make-dereg-rsp-from-dereg-req dereg-req)
  (let ((rc (bytes->uint32 (list-tail (car (filter m3ua-routing-context-parameter? (m3ua-get-parameters-of-message dereg-req)))
				      m3ua-parameter-header-length))))
    (m3ua-make-dereg-rsp-message (list (m3ua-make-deregistration-result-parameter
					(list (m3ua-make-routing-context-parameter (list rc))
					      (m3ua-make-deregistration-status-parameter m3ua-successfully-deregistered-dereg-status)))))))

;;;(m3ua-make-dereg-rsp-from-dereg-req (m3ua-make-dereg-req-message (list (m3ua-make-routing-context-parameter (list 1 2 3)))))



(define (m3ua-make-simple-reg-rsp-message id status context)
  (m3ua-make-reg-rsp-message (list (m3ua-make-registration-result-parameter
				    (list (m3ua-make-local-routing-key-identifier-parameter id)
					  (m3ua-make-registration-status-parameter status)
					  (m3ua-make-routing-context-parameter (list context)))))))
;;; (m3ua-make-simple-reg-rsp-message 1 0 0)


(define (m3ua-get-routing-context-from-reg-rsp reg-rsp)
  (bytes->uint32 (list-tail (car (filter m3ua-routing-context-parameter?
					 (m3ua-get-parameters-of-parameter 
					  (car (filter m3ua-registration-result-parameter? (m3ua-get-parameters-of-message reg-rsp))))))
			    m3ua-parameter-header-length)))
;;; (m3ua-get-routing-context-from-reg-rsp (m3ua-make-simple-reg-rsp-message 1 2 6))


(define (m3ua-get-error-code-from-message msg)
  (m3ua-get-error-code-from-parameter (car (filter m3ua-error-code-parameter? (m3ua-get-parameters msg)))))
;;;(m3ua-get-error-code-from-message (m3ua-make-error-message m3ua-unexpected-message-error-code))


(define (m3ua-get-status-type-from-message msg)
  (m3ua-get-status-type-from-parameter (car (filter m3ua-status-parameter? (m3ua-get-parameters msg)))))
;;;(m3ua-get-status-type-from-message (m3ua-make-notify-message 2 3))


(define (m3ua-get-status-info-from-message msg)
  (m3ua-get-status-info-from-parameter (car (filter m3ua-status-parameter? (m3ua-get-parameters msg)))))
;;;(m3ua-get-status-info-from-message (m3ua-make-notify-message 2 3))



;;;
;;; General accessor function for parameters
;;;

(define m3ua-parameter-tag-offset    0)
(define m3ua-parameter-length-offset 2)
(define m3ua-parameter-value-offset  4)

(define (m3ua-get-parameter-tag l)
  (bytes->uint16 (list-tail l m3ua-parameter-tag-offset)))
;;; (m3ua-get-parameter-tag (m3ua-make-parameter 1 (list 1 2 3)))

(define (m3ua-get-parameter-length l)
  (bytes->uint16 (list-tail l m3ua-parameter-length-offset)))
;;; (m3ua-get-parameter-length (m3ua-make-parameter 1 (list 1 2 3)))

(define (m3ua-get-parameter-value l)
  (list-tail (list-head l (m3ua-get-parameter-length l))  m3ua-parameter-value-offset))
;;; (m3ua-get-parameter-value (m3ua-make-parameter 1 (list 1 2 3)))

(define (m3ua-get-parameter-padding l)
  (list-tail l (m3ua-get-parameter-length l)))
;;; (m3ua-get-parameter-padding (m3ua-make-parameter 1 (list  1 2 3 4)))
 

;;;
;;;  M3UA helper routines
;;;

(define m3ua-maximum-message-length (expt 2 16))

(define (m3ua-connect local-addr local-port remote-addr remote-port)
  (let ((s (socket AF_INET SOCK_STREAM IPPROTO_SCTP)))
    (catch 'system-error
	   (lambda ()
	     (bind s AF_INET (inet-pton AF_INET local-addr) local-port)
	     (connect s AF_INET (inet-pton AF_INET remote-addr) remote-port)
	     (if (defined? 'SCTP_NODELAY)
		 (setsockopt s IPPROTO_SCTP SCTP_NODELAY 1))
	     s)
	   (lambda (key . args)
	     (close s)))))

;;; (m3ua-connect "127.0.0.1" 0 "127.0.0.1" m3ua-port)

(define (m3ua-accept local-addr local-port)
  (let ((s (socket AF_INET SOCK_STREAM IPPROTO_SCTP)))
    (catch 'system-error  
	   (lambda ()
	     (bind s AF_INET (inet-pton AF_INET local-addr) local-port)
	     (listen s 1)
	     (let ((ss (car (accept s))))
	       (close s)
	       (if (defined? 'SCTP_NODELAY)
		   (setsockopt ss IPPROTO_SCTP SCTP_NODELAY 1))
	       ss))
	   (lambda (key . args)
	     (close s)))))
    

;;;(m3ua-accept "127.0.0.1" m3ua-port)

(define (m3ua-send-message socket stream message)
  (catch 'system-error
	 (lambda()
	   (sctp-sendmsg socket (bytes->string message) (htonl m3ua-ppid) stream 0 0 AF_INET INADDR_ANY 0))
	 (lambda (key . args)
	   0)))

(define (m3ua-recv-message socket)
  (let ((buffer (make-string m3ua-maximum-message-length)))
    (catch 'system-error
	   (lambda ()
	     (let ((n (recv! socket buffer)))
	       (string->bytes (substring buffer 0 n))))
	   (lambda (key . args)
	     (list)))))

;;; (m3ua-recv-message s)
(define (m3ua-recv-message-with-timeout socket seconds)
  (let ((buffer (make-string m3ua-maximum-message-length)))
    (catch 'system-error
	   (lambda ()
	     (let ((result (select (list socket) (list) (list) seconds)))
	       (if (null? (car result))
		   (list)
		   (let ((n (recv! socket buffer)))
		     (string->bytes (substring buffer 0 n))))))
	   (lambda (key . args)
	     (list)))))

;;; (m3ua-recv-message-with-timeout s 2)

(define (m3ua-wait-for-message socket predicate)
  (let ((m (m3ua-recv-message socket)))
    (if (or (zero? (length m)) (predicate m))
	m
	(m3ua-wait-for-message socket predicate))))

(define (m3ua-wait-for-message-with-timeout socket predicate seconds)
  (let ((m (m3ua-recv-message-with-timeout socket seconds)))
    (if (or (zero? (length m)) (predicate m))
	m
	(m3ua-wait-for-message-with-timeout socket predicate seconds))))

(define (m3ua-version-ok? version)
  (= version m3ua-version))
;;; (m3ua-version-ok? m3ua-version)
;;; (m3ua-version-ok? (+ m3ua-version 1))

(define (m3ua-message-class-ok? class rkm-message-class-supported?)
  (or (= class m3ua-mgmt-message-class)
      (= class m3ua-tfer-message-class)
      (= class m3ua-ssnm-message-class)
      (= class m3ua-aspsm-message-class)
      (= class m3ua-asptm-message-class)
      (and rkm-message-class-supported? (= class m3ua-rkm-message-class))))
;;; (m3ua-message-class-ok? m3ua-mgmt-message-class #t)
;;; (m3ua-message-class-ok? m3ua-rkm-message-class #t)
;;; (m3ua-message-class-ok? m3ua-rkm-message-class #f)
;;; (m3ua-message-class-ok? 1000)

(define (m3ua-message-type-ok? class type)
  (cond
    ((= class m3ua-mgmt-message-class)
     (or (= type m3ua-err-message-type)
	 (= type m3ua-ntfy-message-type)))
    ((= class m3ua-tfer-message-class)
     (or (= type m3ua-data-message-type)))
    ((= class m3ua-ssnm-message-class)
     (or (= type m3ua-duna-message-type)
	 (= type m3ua-dava-message-type)
	 (= type m3ua-daud-message-type)
	 (= type m3ua-scon-message-type)
	 (= type m3ua-dupu-message-type)
	 (= type m3ua-drst-message-type)))
    ((= class m3ua-aspsm-message-class)
     (or (= type m3ua-aspup-message-type)
	 (= type m3ua-aspdn-message-type)
	 (= type m3ua-beat-message-type)
	 (= type m3ua-aspup-ack-message-type)
	 (= type m3ua-aspdn-ack-message-type)
	 (= type m3ua-beat-ack-message-type)))
    ((= class m3ua-asptm-message-class)
     (or (= type m3ua-aspac-message-type)
	 (= type m3ua-aspia-message-type)
	 (= type m3ua-aspac-ack-message-type)
	 (= type m3ua-aspia-ack-message-type)))
    ((= class m3ua-rkm-message-class)
     (or (= type m3ua-reg-req-message-type)
	 (= type m3ua-reg-rsp-message-type)
	 (= type m3ua-dereg-req-message-type)
	 (= type m3ua-dereg-rsp-message-type)))))

;;; (m3ua-message-type-ok? m3ua-aspsm-message-class 7)

(define (m3ua-check-common-header fd message rkm-message-class-supported?)
  (if (not (m3ua-version-ok? (m3ua-get-version message)))
      (begin
	(m3ua-send-message fd 0 (m3ua-make-error-message m3ua-invalid-version-error-code))
	#f)
      (if (not (m3ua-message-class-ok? (m3ua-get-message-class message) rkm-message-class-supported?))
	  (begin
	    (m3ua-send-message fd 0 (m3ua-make-error-message m3ua-unsupported-message-class-error-code))
	    #f)
	  (if (not (m3ua-message-type-ok? (m3ua-get-message-class message)
					  (m3ua-get-message-type message)))
	      (begin
		(m3ua-send-message fd 0 (m3ua-make-error-message m3ua-unsupported-message-type-error-code))
		#f)
	      #t))))

(define (m3ua-data-message? message)
  (and (= (m3ua-get-message-class message) m3ua-tfer-message-class)
       (= (m3ua-get-message-type message)  m3ua-data-message-type)))
;;; (m3ua-data-message? (m3ua-make-data-message 1 2 3 4 5 6 (list 1 2) (list)))
;;; (m3ua-data-message? (m3ua-make-asp-up-message (list)))

(define (m3ua-error-message? message)
  (and (= (m3ua-get-message-class message) m3ua-mgmt-message-class)
       (= (m3ua-get-message-type message)  m3ua-err-message-type)))
;;; (m3ua-error-message? (m3ua-make-error-message m3ua-unexpected-message-error-code))
;;; (m3ua-error-message? (m3ua-make-asp-up-message (list)))

(define (m3ua-notify-message? message)
  (and (= (m3ua-get-message-class message) m3ua-mgmt-message-class)
       (= (m3ua-get-message-type message)  m3ua-ntfy-message-type)))
;;; (m3ua-notify-message? (m3ua-make-notify-message m3ua-as-state-change-status-type m3ua-as-inactive))
;;; (m3ua-notify-message? (m3ua-make-asp-up-message (list)))

(define (m3ua-beat-message? message)
  (and (= (m3ua-get-message-class message) m3ua-aspsm-message-class)
       (= (m3ua-get-message-type message)  m3ua-beat-message-type)))
;;; (m3ua-beat-message? (m3ua-make-beat-message (list 1 2 3)))
;;; (m3ua-beat-message? (m3ua-make-asp-up-message (list)))

(define (m3ua-beat-ack-message? message)
  (and (= (m3ua-get-message-class message) m3ua-aspsm-message-class)
       (= (m3ua-get-message-type message)  m3ua-beat-ack-message-type)))
;;; (m3ua-beat-ack-message? (m3ua-make-beat-ack-message (list 1 2 3)))
;;; (m3ua-beat-ack-message? (m3ua-make-asp-up-message (list)))

(define (m3ua-asp-up-message? message)
  (and (= (m3ua-get-message-class message) m3ua-aspsm-message-class)
       (= (m3ua-get-message-type message)  m3ua-aspup-message-type)))
;;; (m3ua-asp-up-message? (m3ua-make-asp-up-message (list)))
;;; (m3ua-asp-up-message? (m3ua-make-asp-down-message))

(define (m3ua-asp-up-ack-message? message)
  (and (= (m3ua-get-message-class message) m3ua-aspsm-message-class)
       (= (m3ua-get-message-type message)  m3ua-aspup-ack-message-type)))
;;; (m3ua-asp-up-ack-message? (m3ua-make-asp-up-ack-message))
;;; (m3ua-asp-up-ack-message? (m3ua-make-asp-down-message))

(define (m3ua-asp-active-message? message)
  (and (= (m3ua-get-message-class message) m3ua-asptm-message-class)
       (= (m3ua-get-message-type message)  m3ua-aspac-message-type)))
;;; (m3ua-asp-active-message? (m3ua-make-asp-active-message (list)))
;;; (m3ua-asp-active-message? (m3ua-make-asp-down-message))

(define (m3ua-asp-active-ack-message? message)
  (and (= (m3ua-get-message-class message) m3ua-asptm-message-class)
       (= (m3ua-get-message-type message)  m3ua-aspac-ack-message-type)))
;;; (m3ua-asp-active-ack-message? (m3ua-make-asp-active-ack-message (list)))
;;; (m3ua-asp-active-ack-message? (m3ua-make-asp-down-message))

(define (m3ua-asp-down-message? message)
  (and (= (m3ua-get-message-class message) m3ua-aspsm-message-class)
       (= (m3ua-get-message-type message)  m3ua-aspdn-message-type)))
;;; (m3ua-asp-down-message? (m3ua-make-asp-down-message))
;;; (m3ua-asp-down-message? (m3ua-make-asp-up-message (list)))

(define (m3ua-asp-down-ack-message? message)
  (and (= (m3ua-get-message-class message) m3ua-aspsm-message-class)
       (= (m3ua-get-message-type message)  m3ua-aspdn-ack-message-type)))
;;; (m3ua-asp-down-ack-message? (m3ua-make-asp-down-ack-message))
;;; (m3ua-asp-down-ack-message? (m3ua-make-asp-up-message (list)))

(define (m3ua-asp-inactive-message? message)
  (and (= (m3ua-get-message-class message) m3ua-asptm-message-class)
       (= (m3ua-get-message-type message)  m3ua-aspia-message-type)))
;;; (m3ua-asp-inactive-message? (m3ua-make-asp-inactive-message (list)))
;;; (m3ua-asp-inactive-message? (m3ua-make-asp-down-message))

(define (m3ua-asp-inactive-ack-message? message)
  (and (= (m3ua-get-message-class message) m3ua-asptm-message-class)
       (= (m3ua-get-message-type message)  m3ua-aspia-ack-message-type)))
;;; (m3ua-asp-inactive-ack-message? (m3ua-make-asp-inactive-ack-message (list)))
;;; (m3ua-asp-inactive-ack-message? (m3ua-make-asp-down-message))

(define (m3ua-daud-message? message)
  (and (= (m3ua-get-message-class message) m3ua-ssnm-message-class)
       (= (m3ua-get-message-type message)  m3ua-daud-message-type)))
;;; (m3ua-daud-message? (m3ua-make-daud-message (list)))
;;; (m3ua-daud-message? (m3ua-make-asp-down-message))

(define (m3ua-duna-message? message)
  (and (= (m3ua-get-message-class message) m3ua-ssnm-message-class)
       (= (m3ua-get-message-type message)  m3ua-duna-message-type)))
;;; (m3ua-duna-message? (m3ua-make-duna-message (list)))
;;; (m3ua-duna-message? (m3ua-make-asp-down-message))

(define (m3ua-dava-message? message)
  (and (= (m3ua-get-message-class message) m3ua-ssnm-message-class)
       (= (m3ua-get-message-type message)  m3ua-dava-message-type)))
;;; (m3ua-dava-message? (m3ua-make-dava-message (list)))
;;; (m3ua-dava-message? (m3ua-make-asp-down-message))

(define (m3ua-drst-message? message)
  (and (= (m3ua-get-message-class message) m3ua-ssnm-message-class)
       (= (m3ua-get-message-type message)  m3ua-drst-message-type)))
;;; (m3ua-drst-message? (m3ua-make-drst-message (list)))
;;; (m3ua-drst-message? (m3ua-make-asp-down-message))

(define (m3ua-scon-message? message)
  (and (= (m3ua-get-message-class message) m3ua-ssnm-message-class)
       (= (m3ua-get-message-type message)  m3ua-scon-message-type)))
;;; (m3ua-scon-message? (m3ua-make-scon-message (list)))
;;; (m3ua-scon-message? (m3ua-make-asp-down-message))

(define (m3ua-reg-req-message? message)
  (and (= (m3ua-get-message-class message) m3ua-rkm-message-class)
       (= (m3ua-get-message-type message)  m3ua-reg-req-message-type)))
;;; (m3ua-reg-req-message? (m3ua-make-reg-req-message (list)))
;;; (m3ua-reg-req-message? (m3ua-make-asp-down-message))

(define (m3ua-reg-rsp-message? message)
  (and (= (m3ua-get-message-class message) m3ua-rkm-message-class)
       (= (m3ua-get-message-type message)  m3ua-reg-rsp-message-type)))
;;; (m3ua-reg-rsp-message? (m3ua-make-reg-rsp-message (list)))
;;; (m3ua-reg-rsp-message? (m3ua-make-asp-down-message))

(define (m3ua-dereg-req-message? message)
  (and (= (m3ua-get-message-class message) m3ua-rkm-message-class)
       (= (m3ua-get-message-type message)  m3ua-dereg-req-message-type)))
;;; (m3ua-dereg-req-message? (m3ua-make-dereg-req-message (list)))
;;; (m3ua-dereg-req-message? (m3ua-make-asp-down-message))

(define (m3ua-dereg-rsp-message? message)
  (and (= (m3ua-get-message-class message) m3ua-rkm-message-class)
       (= (m3ua-get-message-type message)  m3ua-dereg-rsp-message-type)))
;;; (m3ua-dereg-rsp-message? (m3ua-make-dereg-rsp-message (list)))
;;; (m3ua-dereg-rsp-message? (m3ua-make-asp-down-message))

(define m3ua-asp-down           0)
(define m3ua-asp-inactive       1)
(define m3ua-asp-active         2)
(define m3ua-asp-reflect-beat   3)
(define m3ua-asp-send-data      4)
(define m3ua-asp-receive-data   5)
(define m3ua-asp-send-reg-req   6)
(define m3ua-asp-send-dereg-req 7)

(define (m3ua-handle-sgp-message fd state rkm-message-class-supported?)
  (let ((message (m3ua-recv-message fd)))
    (if (positive? (length message))
	(if (m3ua-check-common-header fd message rkm-message-class-supported?)
	    (cond 
	     ((m3ua-beat-message? message)
	      (m3ua-send-message fd 0 (m3ua-make-message m3ua-aspsm-message-class
							 m3ua-beat-ack-message-type
							 (m3ua-get-parameters message)))
	      (m3ua-handle-sgp-message fd state rkm-message-class-supported?))

	     ((m3ua-asp-up-message? message)
	      (if (= state m3ua-asp-active)
		  (m3ua-send-message fd 0 (m3ua-make-error-message m3ua-unexpected-message-error-code)))
	      (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
	      (if (not (= state m3ua-asp-inactive))
		  (m3ua-send-message fd 0 (m3ua-make-notify-message m3ua-as-state-change-status-type
								    m3ua-as-inactive)))
	      (m3ua-handle-sgp-message fd m3ua-asp-inactive rkm-message-class-supported?))
	     
	     ((m3ua-asp-active-message? message)
	      (if (= state m3ua-asp-down)
		  (begin
		    (m3ua-send-message fd 0 (m3ua-make-error-message m3ua-unexpected-message-error-code))
		    (m3ua-handle-sgp-message fd m3ua-asp-down rkm-message-class-supported?))
		  (begin
		    (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters message)))
		    (if (not (= state m3ua-asp-active))
			(m3ua-send-message fd 0 (m3ua-make-notify-message m3ua-as-state-change-status-type
									  m3ua-as-active)))
		    (m3ua-handle-sgp-message fd m3ua-asp-active rkm-message-class-supported?))))
	     
	     ((m3ua-asp-down-message? message)
	      (m3ua-send-message fd 0 (m3ua-make-asp-down-ack-message))
	      (m3ua-handle-sgp-message fd m3ua-asp-down rkm-message-class-supported?))
	     
	     ((m3ua-asp-inactive-message? message)
	      (if (= state m3ua-asp-down)
		  (begin
		    (m3ua-send-message fd 0 (m3ua-make-asp-down-ack-message))
		    (m3ua-handle-sgp-message fd m3ua-asp-down rkm-message-class-supported?))
		  (begin
		    (m3ua-send-message fd 0 (m3ua-make-asp-inactive-ack-message (list)))
		    (if (= state m3ua-asp-active)
			(m3ua-send-message fd 0 (m3ua-make-notify-message m3ua-as-state-change-status-type
									  m3ua-as-pending)))
		    (m3ua-handle-sgp-message fd m3ua-asp-inactive rkm-message-class-supported?))))
	     ((m3ua-reg-req-message? message)
	      (if (= state m3ua-asp-inactive)
		  (m3ua-send-message fd 0 (m3ua-make-reg-rsp-from-reg-req message))
		  (m3ua-send-message fd 0 (m3ua-make-error-message m3ua-unexpected-message-error-code)))
	      (m3ua-handle-sgp-message fd state rkm-message-class-supported?))
	     ((m3ua-dereg-req-message? message)
	      (m3ua-send-message fd 0 (m3ua-make-dereg-rsp-from-dereg-req message))
	      (m3ua-handle-sgp-message fd state rkm-message-class-supported?))
	     (else
	      (m3ua-send-message fd 0 (m3ua-make-error-message m3ua-unexpected-message-error-code))
	      (m3ua-handle-sgp-message fd state rkm-message-class-supported?)))))))

(define (m3ua-run-sgp port rkm-message-class-supported?)
  (let ((fd (m3ua-accept "0.0.0.0" port)))
    (m3ua-handle-sgp-message fd m3ua-asp-down rkm-message-class-supported?)
    (close fd)))
;;; (m3ua-run-sgp m3ua-port #t) ;;; RKM message class supported
;;; (m3ua-run-sgp m3ua-port #f) ;;; RKM message class not supported




(define (m3ua-perform-asp-states fd current-state states)
  (if (null? states)
      (close fd)
      (cond
	((= (car states) m3ua-asp-down)
 	 (m3ua-send-message fd 0 (m3ua-make-asp-down-message))
	 (let ((message (m3ua-recv-message fd)))
	   (if (positive? (length message))
	       (if (m3ua-check-common-header fd message #t)
		   (if (m3ua-asp-down-ack-message? message)
		       (m3ua-perform-asp-states fd m3ua-asp-down (cdr states))
		       (close fd))
		   (close fd)))
	   (close fd)))
	((= (car states) m3ua-asp-inactive)
	 (if (= current-state m3ua-asp-down)
	     (begin
	       (m3ua-send-message fd 0 (m3ua-make-asp-up-message (list)))
	       (let ((message (m3ua-recv-message fd)))
		 (if (positive? (length message))
		     (if (m3ua-check-common-header fd message #t)
			 (if (m3ua-asp-up-ack-message? message)
			     (m3ua-perform-asp-states fd m3ua-asp-inactive (cdr states))
			     (close fd))
			 (close fd))
		     (close fd))))
	     (begin
	       (m3ua-send-message fd 0 (m3ua-make-asp-inactive-message (list)))
	       (let ((message (m3ua-recv-message fd)))
		 (if (positive? (length message))
		     (if (m3ua-check-common-header fd message #t)
			 (if (m3ua-asp-inactive-ack-message? message)
			     (m3ua-perform-asp-states fd m3ua-asp-inactive (cdr states))
			     (close fd))
			 (close fd))
		     (close fd))))))
	((= (car states) m3ua-asp-active)
	 (m3ua-send-message fd 0 (m3ua-make-asp-active-message (list)))
	 (let ((message (m3ua-recv-message fd)))
	   (if (positive? (length message))
	       (if (m3ua-check-common-header fd message #t)
		   (if (m3ua-asp-active-ack-message? message)
		       (m3ua-perform-asp-states fd m3ua-asp-active (cdr states))
		       (close fd))
		   (close fd))
	       (close fd))))
	((= (car states) m3ua-asp-reflect-beat)
	 (let ((message (m3ua-recv-message fd)))
	   (if (positive? (length message))
	       (if (m3ua-check-common-header fd message #t)
		   (if (m3ua-beat-message? message)
		       (begin
			 (m3ua-send-message fd 0 (m3ua-make-beat-ack-message (m3ua-get-parameter-value (car (m3ua-get-parameters message)))))
			 (m3ua-perform-asp-states fd current-state (cdr states)))
		       (m3ua-perform-asp-states fd current-state states))
		   (close fd))
	       (close fd))))
	((= (car states) m3ua-asp-send-data)
	 (m3ua-send-message fd 1 (m3ua-make-data-message opc dpc si ni mp sls ss7-message data-message-parameters))
	 (m3ua-perform-asp-states fd current-state (cdr states)))
	((= (car states) m3ua-asp-receive-data)
	 (let ((message (m3ua-recv-message fd)))
	   (if (positive? (length message))
	       (if (m3ua-check-common-header fd message #t)
		   (m3ua-perform-asp-states fd current-state (cdr states))
		   (close fd))
	       (close fd))))
	((= (car states) m3ua-asp-send-reg-req)
	 (m3ua-send-message fd 0 (m3ua-make-reg-req-message 
				  (list (m3ua-make-routing-key-parameter
					 (list (m3ua-make-local-routing-key-identifier-parameter 1)
					       (m3ua-make-destination-point-code-parameter 2))))))
	 (let ((message (m3ua-recv-message fd)))
	   (if (positive? (length message))
	       (if (m3ua-check-common-header fd message #t)
		   (m3ua-perform-asp-states fd current-state (cdr states))
		   (close fd))
	       (close fd))))
	((= (car states) m3ua-asp-send-dereg-req)
	 (m3ua-send-message fd 0 (m3ua-make-dereg-req-message (list (m3ua-make-routing-context-parameter (list 1)))))
	 (let ((message (m3ua-recv-message fd)))
	   (if (positive? (length message))
	       (if (m3ua-check-common-header fd message #t)
		   (m3ua-perform-asp-states fd current-state (cdr states))
		   (close fd))
	       (close fd))))
	(else
	 (error 'wrong-state)))))

(define (m3ua-run-asp remote-addr states)
  (let ((fd (m3ua-connect "0.0.0.0" 0 remote-addr m3ua-port)))
    (m3ua-perform-asp-states fd m3ua-asp-down states)))

(define (m3ua-send-beats local-addr local-port remote-addr remote-port number length)
  (let ((fd (m3ua-connect local-addr local-port remote-addr remote-port))
	(beat-message (m3ua-make-beat-message (random-bytes length))))
    (dotimes (n number)
	     (m3ua-send-message fd 0 beat-message)
	     (m3ua-recv-message fd))
    (sleep 1)
    (close fd)))
;;; (m3ua-send-beats "192.168.1.2" m3ua-port "192.168.1.8" m3ua-port 1000 1000)
