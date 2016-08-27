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

;;; $Id: m3ua-sgp-tests.scm,v 1.9 2012/08/28 19:56:13 tuexen Exp $

;;; Version 1.3.0
;;;
;;; History
;;; 04.12.2004: Fix name test-addr- -> tester-addr in almost all testcases
;;; 04.12.2004: Fix name of m3ua-sgp-mtr-v-001 to m3ua-sgp-mtr-v-002.
;;; 06.12.2004: Move SUT parameter to external file.
;;; 06.12.2004: Use asp-up-message-parameters as default last arg of m3ua-make-asp-up-message.
;;; 06.12.2004: Use asp-active-message-parameters as default last arg of m3ua-make-asp-active-message.
;;; 06.12.2004: Use asp-active-ack-message-parameters as default last arg of m3ua-make-asp-active-ack-message.
;;; 06.12.2004: Use asp-inactive-message-parameters as default last arg of m3ua-make-asp-inactxive-message.
;;; 06.12.2004: Use asp-inactive-ack-message-parameters as default last arg of m3ua-make-asp-inactive-ack-message.
;;; 06.12.2004: Use data-message-parameters as default last arg of m3ua-make-data-message.
;;; 09.12.2004: m3ua-sgp-mtr-v-00[23] implemented according to change request.
;;; 14.12.2004: m3ua-sgp-aspsm-v-009 added.
;;; 14.12.2004: m3ua-sgp-asptm-i-003 added.
;;; 18.12.2004: Use iut-ni iut-mp and iut-sls in m3ua-make-data-message.
;;; 19.12.2004: m3ua-sgp-asptm-v-014 added.
;;; 19.12.2004: m3ua-sgp-asptm-v-015 added.
;;; 19.12.2004: m3ua-sgp-asptm-i-009 added.
;;; 19.12.2004: m3ua-sgp-mtr-v-001 added.
;;; 19.12.2004: m3ua-sgp-mtr-v-002 additional variant added.
;;; 19.12.2004: m3ua-sgp-mtr-v-003 additional variant added.
;;; 13.09.2005: Implement ASP tests.
;;; 18.02.2006: Implement m3ua-sgp-rkm*
;;; 12.03.2006: Fix name of m3ua-sgp-rkm-v-02[123] to m3ua-sgp-rkm-i-02[123]
;;; 27.08.2006: m3ua-sgp-mtr-v-00[12]: do not send data before reception of DAVA. Should I send DAUD?


;;;
;;; Definition of the tests for the SGP
;;;

(define (m3ua-sgp-aspsm-v-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-sgp-aspsm-v-001 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ASPUP-ACK is returned



(define (m3ua-sgp-aspsm-v-003 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (let ((msg (m3ua-wait-for-message fd m3ua-notify-message?)))
      (close fd)
      (if (and (= (m3ua-get-status-type-from-message msg) m3ua-as-state-change-status-type)
	       (= (m3ua-get-status-info-from-message msg) m3ua-as-inactive))
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-aspsm-v-003 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is a ASPUP-ACK and a NOTIFY(AS_INACTIVE)



(define (m3ua-sgp-aspsm-v-005 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-down-message))
    (m3ua-wait-for-message fd m3ua-asp-down-ack-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-sgp-aspsm-v-005 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is a ASPDN-ACK



(define (m3ua-sgp-aspsm-v-009 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-refused-management-blocking-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-aspsm-v-009 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(Refused - Management Blocking)
;;; is returned. Of course, the ASP has to be configured appropiately at the SUT.



(define (m3ua-sgp-aspsm-i-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-common-header (+ m3ua-version 1)
						     m3ua-reserved
						     m3ua-aspsm-message-class
						     m3ua-aspup-message-type
						     m3ua-common-header-length))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-invalid-version-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-aspsm-i-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is a ERROR(invalid version)



(define (m3ua-sgp-aspsm-i-002 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-common-header m3ua-version
						     m3ua-reserved
						     m3ua-aspsm-message-class
						     m3ua-reserved-aspsm-message-type
						     m3ua-common-header-length))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-unsupported-message-type-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-aspsm-i-002 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is a ERROR(unsupported message type)



(define (m3ua-sgp-aspsm-i-003 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-unexpected-message-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-aspsm-i-003 tester-addr tester-port sut-addr sut-port)
;;; This test needs clarification. FIXME.



(define (m3ua-sgp-aspsm-i-004 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-common-header m3ua-version
						     m3ua-reserved
						     m3ua-aspsm-message-class
						     m3ua-reserved-aspsm-message-type
						     m3ua-common-header-length))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-unsupported-message-type-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-aspsm-i-004 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unsupported message type)



(define (m3ua-sgp-aspsm-o-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-sgp-aspsm-o-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPUP-ACK.



(define (m3ua-sgp-aspsm-o-003 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-unexpected-message-error-code)
	  (begin
	    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
	    (let ((msg (m3ua-wait-for-message fd m3ua-notify-message?)))
	      (close fd)
	      (if (and (= (m3ua-get-status-type-from-message msg) m3ua-as-state-change-status-type)
		       (= (m3ua-get-status-info-from-message msg) m3ua-as-inactive))
		  m3ua-test-result-passed
		  m3ua-test-result-failed)))
	  (begin 
	    (close fd)
	    m3ua-test-result-failed)))))
;;; (m3ua-sgp-aspsm-o-003 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unexpected message),
;;; an  ASPUP-ACK and a NOTIFY(AS_INACTIVE).



(define (m3ua-sgp-aspsm-o-004 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-down-message))
    (m3ua-wait-for-message fd m3ua-asp-down-ack-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-sgp-aspsm-o-004 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPDN-ACK,



(define (m3ua-sgp-asptm-v-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-sgp-asptm-v-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPAC-ACK.



(define (m3ua-sgp-asptm-v-003 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (let ((msg (m3ua-wait-for-message fd m3ua-notify-message?)))
      (close fd)
      (if (and (= (m3ua-get-status-type-from-message msg) m3ua-as-state-change-status-type)
	       (= (m3ua-get-status-info-from-message msg) m3ua-as-active))
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-asptm-v-003 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPAC-ACK and NOTIFY(AS-ACTIVE).



(define (m3ua-sgp-asptm-v-005 tester-addr tester-port sut-addr sut-port rc)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message (list (m3ua-make-routing-context-parameter (list rc)))))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (close fd)
    m3ua-test-result-unknown))
;;; (m3ua-sgp-asptm-v-005 tester-addr tester-port sut-addr sut-port tester-rc-valid)
;;; This test is passed if there is an ASPAC-ACK contains the RC.
;;; NOTE: This test does not use the asp-active-message-parameters variable.


(define (m3ua-sgp-asptm-v-006 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-inactive-message asp-inactive-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-inactive-ack-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-sgp-asptm-v-006 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPIA-ACK.



(define (m3ua-sgp-asptm-v-008 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-inactive-message asp-inactive-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-inactive-ack-message?)
    (let ((msg (m3ua-wait-for-message fd m3ua-notify-message?)))
      (close fd)
      (if (and (= (m3ua-get-status-type-from-message msg) m3ua-as-state-change-status-type)
	       (= (m3ua-get-status-info-from-message msg) m3ua-as-pending))
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-asptm-v-008 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPIA-ACK and NOTIFY(AS-PENDING).



(define (m3ua-sgp-asptm-v-010 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-beat-message (string->bytes "M3UA rocks")))
    (m3ua-wait-for-message fd m3ua-beat-ack-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-sgp-asptm-v-010 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is a BEAT-ACK.



(define (m3ua-sgp-asptm-v-011 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (let ((value (random-bytes 13)))
      (m3ua-send-message fd 0  (m3ua-make-beat-message value))
      (let ((msg (m3ua-wait-for-message fd m3ua-beat-ack-message?)))
	(close fd)
	(if (equal? msg (m3ua-make-beat-ack-message value))
	    m3ua-test-result-passed
	    m3ua-test-result-failed)))))
;;; (m3ua-sgp-asptm-v-011 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is a BEAT-ACK with unchanged data.



(define (m3ua-sgp-asptm-v-013 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 asp-id-1 asp-id-2)
  (let ((fd1 (m3ua-connect tester-addr tester-port-1 sut-addr sut-port-1))
	(fd2 (m3ua-connect tester-addr tester-port-2 sut-addr sut-port-2)))
    ;;; Move ASP1 to ASP-ACTIVE
    (m3ua-send-message fd1 0 (m3ua-make-asp-up-message (list (m3ua-make-asp-id-parameter asp-id-1))))
    (m3ua-wait-for-message fd1 m3ua-asp-up-ack-message?)
    (m3ua-send-message fd1 0 (m3ua-make-asp-active-message (list (m3ua-make-traffic-mode-type-parameter m3ua-traffic-mode-type-broadcast))))
    (m3ua-wait-for-message fd1 m3ua-asp-active-ack-message?)
    ;;; Move ASP2 to ASP-ACTIVE
    (m3ua-send-message fd2 0 (m3ua-make-asp-up-message (list (m3ua-make-asp-id-parameter asp-id-2))))
    (m3ua-wait-for-message fd2 m3ua-asp-up-ack-message?)
    (m3ua-send-message fd2 0 (m3ua-make-asp-active-message (list (m3ua-make-traffic-mode-type-parameter m3ua-traffic-mode-type-broadcast))))
    (m3ua-wait-for-message fd2 m3ua-asp-active-ack-message?)
    ;;; Now move ASP1 to ASP-INACTIVE
    (m3ua-send-message fd1 0 (m3ua-make-asp-inactive-message (list)))
    (m3ua-wait-for-message fd1 m3ua-asp-inactive-ack-message?)
    (let ((msg (m3ua-wait-for-message fd1 m3ua-notify-message?)))
      (close fd1)
      (close fd2)
      (if (and (= (m3ua-get-status-type-from-message msg) m3ua-other-status-type)
	       (= (m3ua-get-status-info-from-message msg) m3ua-insufficient-resources))
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-asptm-v-013 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 asp-id-1 asp-id-2)
;;; This test is passed if the SUT sends a NOTIFY.



(define (m3ua-sgp-asptm-v-014 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 asp-id-1 asp-id-2)
  (let ((fd1 (m3ua-connect tester-addr tester-port-1 sut-addr sut-port-1))
	(fd2 (m3ua-connect tester-addr tester-port-2 sut-addr sut-port-2)))
    ;;; Move ASP1 to ASP-INACTIVE
    (m3ua-send-message fd1 0 (m3ua-make-asp-up-message (list (m3ua-make-asp-id-parameter asp-id-1))))
    (m3ua-wait-for-message fd1 m3ua-asp-up-ack-message?)
    ;;; Move ASP2 to ASP-ACTIVE
    (m3ua-send-message fd2 0 (m3ua-make-asp-up-message (list (m3ua-make-asp-id-parameter asp-id-2))))
    (m3ua-wait-for-message fd2 m3ua-asp-up-ack-message?)
    (m3ua-send-message fd2 0 (m3ua-make-asp-active-message (list (m3ua-make-traffic-mode-type-parameter m3ua-traffic-mode-type-override))))
    (m3ua-wait-for-message fd2 m3ua-asp-active-ack-message?)
    ;;; Now move ASP1 to ASP-ACTIVE
    (m3ua-send-message fd1 0 (m3ua-make-asp-active-message (list (m3ua-make-traffic-mode-type-parameter m3ua-traffic-mode-type-override))))
    (m3ua-wait-for-message fd1 m3ua-asp-active-ack-message?)
    (m3ua-wait-for-message fd2 m3ua-notify-message?)
    (close fd1)
    (close fd2)
    m3ua-test-result-passed))
;;; (m3ua-sgp-asptm-v-014 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 asp-id-1 asp-id-2)
;;; This test is passed if the SUT sends an ASPAC-ACK and a NOTIFY.



(define m3ua-sgp-asptm-v-015 m3ua-sgp-asptm-v-014)
;;; (m3ua-sgp-asptm-v-014 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 asp-id-1 asp-id-2)
;;; This test is passed if the SUT sends an ASPAC-ACK and a NOTIFY including the ASP-ID.



(define (m3ua-sgp-asptm-i-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-common-header (+ m3ua-version 1)
						     m3ua-reserved
						     m3ua-asptm-message-class
						     m3ua-aspac-message-type
						     m3ua-common-header-length))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-invalid-version-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-asptm-i-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(invalid version).



(define (m3ua-sgp-asptm-i-003 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message (list (m3ua-make-traffic-mode-type-parameter m3ua-traffic-mode-type-broadcast))))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-unsupported-traffic-mode-type-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-asptm-i-003 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unsupported traffic mode type).
;;; NOTE: This test does not used the asp-active-message-parameters variable.



(define (m3ua-sgp-asptm-i-004 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message (list (m3ua-make-traffic-mode-type-parameter 4))))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-unsupported-traffic-mode-type-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-asptm-i-004 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unsupported traffic mode type).
;;; NOTE: This test does not used the asp-active-message-parameters variable.



(define (m3ua-sgp-asptm-i-005-help tester-addr tester-port sut-addr sut-port rc)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message (list (m3ua-make-routing-context-parameter (list rc)))))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-invalid-routing-context-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
(define (m3ua-sgp-asptm-i-005 tester-addr tester-port sut-addr sut-port)
  (m3ua-sgp-asptm-i-005-help tester-addr tester-port sut-addr sut-port tester-rc-invalid))
;;; (m3ua-sgp-asptm-i-005 tester-addr tester-port sut-addr sut-port tester-rc-invalid)
;;; This test is passed if there is an ERROR(invalid routing context)..
;;; NOTE: This test does not use the asp-active-message-parameters variabel.



(define (m3ua-sgp-asptm-i-006 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-common-header m3ua-version
						     m3ua-reserved
						     m3ua-asptm-message-class
						     5
						     m3ua-common-header-length))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-unsupported-message-type-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-asptm-i-006 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unsupported message type).



(define (m3ua-sgp-asptm-i-008 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-common-header m3ua-version
						     m3ua-reserved
						     m3ua-asptm-message-class
						     5
						     m3ua-common-header-length))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-unsupported-message-type-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-asptm-i-008 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unsupported message type).



(define (m3ua-sgp-asptm-i-009 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 asp-id-1 asp-id-2)
  (let ((fd1 (m3ua-connect tester-addr tester-port-1 sut-addr sut-port-1))
	(fd2 (m3ua-connect tester-addr tester-port-2 sut-addr sut-port-2)))
    ;;; Move ASP1 to ASP-ACTIVE
    (m3ua-send-message fd1 0 (m3ua-make-asp-up-message (list (m3ua-make-asp-id-parameter asp-id-1))))
    (m3ua-wait-for-message fd1 m3ua-asp-up-ack-message?)
    (m3ua-send-message fd1 0 (m3ua-make-asp-active-message (list (m3ua-make-traffic-mode-type-parameter m3ua-traffic-mode-type-override))))
    (m3ua-wait-for-message fd1 m3ua-asp-active-ack-message?)
    ;;; Move ASP2 to ASP-ACTIVE
    (m3ua-send-message fd2 0 (m3ua-make-asp-up-message (list (m3ua-make-asp-id-parameter asp-id-2))))
    (m3ua-wait-for-message fd2 m3ua-asp-up-ack-message?)
    (m3ua-send-message fd2 0 (m3ua-make-asp-active-message (list (m3ua-make-traffic-mode-type-parameter m3ua-traffic-mode-type-override))))
    (m3ua-wait-for-message fd2 m3ua-asp-active-ack-message?)
    ;;; Now fail communication to ASP1 via SHUTDOWN procedure.
    (close fd1)
    (let ((msg (m3ua-wait-for-message fd2 m3ua-notify-message?)))
      (close fd2)
      (if (and (= (m3ua-get-status-type-from-message msg) m3ua-other-status-type)
	       (= (m3ua-get-status-info-from-message msg) m3ua-asp-failure))
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-asptm-i-009 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 asp-id-1 asp-id-2)
;;; This test is passed if the SUT sends a NOTIFY(ASP-FAILURE).



(define (m3ua-sgp-asptm-i-010 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-inactive-message asp-inactive-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-inactive-ack-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-sgp-asptm-i-010 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPIA-ACK.



(define (m3ua-sgp-asptm-o-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-sgp-asptm-o-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPAC-ACK.



(define (m3ua-sgp-asptm-o-003 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-inactive-message asp-inactive-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-inactive-ack-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-sgp-asptm-o-003 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPIA-ACK.



(define (m3ua-sgp-mtr-v-001 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 rc-1 rc-2 tester-pc-1 tester-pc-2)
  (let ((fd1 (m3ua-connect tester-addr tester-port-1 sut-addr sut-port-1))
	(fd2 (m3ua-connect tester-addr tester-port-2 sut-addr sut-port-2)))
    ;;; Move ASP1 to ASP-ACTIVE
    (m3ua-send-message fd1 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd1 m3ua-asp-up-ack-message?)
    (m3ua-send-message fd1 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd1 m3ua-asp-active-ack-message?)
    ;;; Move ASP2 to ASP-ACTIVE
    (m3ua-send-message fd2 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd2 m3ua-asp-up-ack-message?)
    (m3ua-send-message fd2 0 (m3ua-make-asp-active-message (list (m3ua-make-routing-context-parameter (list rc-1 rc-2)))))
    (m3ua-wait-for-message fd2 m3ua-asp-active-ack-message?)
    (sleep 10) ;;; wait for DAVA
    (do ((sls 0 (+ sls 1)))
	((= sls 16))
      (m3ua-send-message fd1 1 (m3ua-make-data-message tester-pc-1 tester-pc-2 ss7-si iut-ni iut-mp sls ss7-message data-message-parameters))
      (m3ua-wait-for-message fd2 m3ua-data-message?)
      (sleep 1))
    (close fd1)
    (close fd2)
    m3ua-test-result-unknown))
;;; (m3ua-sgp-mtr-v-001 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 tester-rc-valid-1 tester-rc-valid-2 tester-pc-1 tester-pc-2)
;;; tester-pc-1 must be the point code of ASP corresponding to tester-addr tester-port-1 <-> sut-addr sut-port-1
;;; tester-pc-2 must be the point code of ASP corresponding to tester-addr tester-port-2 <-> sut-addr sut-port-2
;;; See ETSI document.



(define (m3ua-sgp-mtr-v-002 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 tester-pc-1 tester-pc-2)
  (let ((fd1 (m3ua-connect tester-addr tester-port-1 sut-addr sut-port-1))
	(fd2 (m3ua-connect tester-addr tester-port-2 sut-addr sut-port-2)))
    ;;; Move ASP1 to ASP-ACTIVE
    (m3ua-send-message fd1 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd1 m3ua-asp-up-ack-message?)
    (m3ua-send-message fd1 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd1 m3ua-asp-active-ack-message?)
    ;;; Move ASP2 to ASP-ACTIVE
    (m3ua-send-message fd2 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd2 m3ua-asp-up-ack-message?)
    (m3ua-send-message fd2 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd2 m3ua-asp-active-ack-message?)
    (sleep 10) ;;; wait for DAVA
    (do ((sls 0 (+ sls 1)))
	((= sls 16))
      (m3ua-send-message fd1 1 (m3ua-make-data-message tester-pc-1 tester-pc-2 ss7-si iut-ni iut-mp sls ss7-message data-message-parameters))
      (m3ua-wait-for-message fd2 m3ua-data-message?)
      (sleep 1))
    (close fd1)
    (close fd2)
    m3ua-test-result-unknown))
;;; (m3ua-sgp-asptm-v-002 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 tester-pc-1 tester-pc-2)
;;; tester-pc-1 must be the point code of ASP corresponding to tester-addr tester-port-1 <-> sut-addr sut-port-1
;;; tester-pc-2 must be the point code of ASP corresponding to tester-addr tester-port-2 <-> sut-addr sut-port-2
;;; See ETSI document.



(define (m3ua-sgp-mtr-v-002-alternate tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 1 (m3ua-make-data-message tester-pc tester-pc ss7-si iut-ni iut-mp iut-sls ss7-message data-message-parameters))
    (m3ua-send-message fd 1 (apply append (cons (m3ua-make-common-header m3ua-version
									 m3ua-reserved
									 m3ua-tfer-message-class
									 m3ua-data-message-type
									 m3ua-common-header-length)
						data-message-parameters)))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-missing-parameter-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-mtr-v-002-alternate tester-addr tester-port sut-addr sut-port)
;;; This test is passed if the SUT responds with an ERROR message to the second DATA message.
;;; FIXME: This does NOT match the current ETSI test but a change request.



(define m3ua-sgp-mtr-v-003 m3ua-sgp-mtr-v-002)
;;; (m3ua-sgp-asptm-v-003 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 tester-pc-1 tester-pc-2)
;;; tester-pc-1 must be the point code of ASP corresponding to tester-addr tester-port-1 <-> sut-addr sut-port-1
;;; tester-pc-2 must be the point code of ASP corresponding to tester-addr tester-port-2 <-> sut-addr sut-port-2
;;; See ETSI document.



(define (m3ua-sgp-mtr-v-003-alternate tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 1 (m3ua-make-data-message tester-pc tester-pc ss7-si iut-ni iut-mp iut-sls ss7-message data-message-parameters))
    (m3ua-send-message fd 0 (m3ua-make-data-message tester-pc tester-pc ss7-si iut-ni iut-mp iut-sls ss7-message data-message-parameters))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-invalid-stream-identifier-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-mtr-v-003-alternate tester-addr tester-port sut-addr sut-port)
;;; This test is passed if the SUT sends an ERROR message for the second DATA message.
;;; FIXME: This does NOT match the current ETSI test but a change request.



(define m3ua-sgp-mtr-v-004 m3ua-sgp-mtr-v-002)
;;; (m3ua-sgp-asptm-v-004 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 tester-pc-1 tester-pc-2)
;;; tester-pc-1 must be the point code of ASP corresponding to tester-addr tester-port-1 <-> sut-addr sut-port-1
;;; tester-pc-2 must be the point code of ASP corresponding to tester-addr tester-port-2 <-> sut-addr sut-port-2
;;; See ETSI document.



(define (m3ua-sgp-mtr-i-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 1 (append (m3ua-make-common-header (+ 1 m3ua-version)
							     m3ua-reserved
							     m3ua-tfer-message-class
							     m3ua-data-message-type
							     (+ m3ua-common-header-length
								m3ua-data-parameter-header-length
								(length ss7-message)))
				    (m3ua-make-data-parameter tester-pc
							      tester-pc
							      ss7-si
							      0
							      0
							      4
							      ss7-message)))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-invalid-version-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-mtr-i-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(invalid version).



(define (m3ua-sgp-mtr-i-002 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 0 (append (m3ua-make-common-header m3ua-version
							     m3ua-reserved
							     10
							     m3ua-data-message-type
							     (+ m3ua-common-header-length 
								m3ua-data-parameter-header-length
								(length ss7-message)))
				    (m3ua-make-data-parameter tester-pc
							      tester-pc
							      ss7-si
							      0
							      0
							      4
							      ss7-message)))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-unsupported-message-class-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-mtr-i-002 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unsupported message class).



(define (m3ua-sgp-mtr-i-003 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 1 (append (m3ua-make-common-header m3ua-version
							     m3ua-reserved
							     m3ua-tfer-message-class
							     2
							     (+ m3ua-common-header-length
								m3ua-data-parameter-header-length
								(length ss7-message)))
				    (m3ua-make-data-parameter tester-pc
							      tester-pc
							      ss7-si
							      0
							      0
							      4
							      ss7-message)))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-unsupported-message-type-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-mtr-i-003 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unsupported message type).



(define (m3ua-sgp-rkm-v-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 1)
				     (m3ua-make-destination-point-code-parameter tester-pc))))))
    (m3ua-wait-for-message fd m3ua-reg-rsp-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-sgp-rkm-v-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if a REG_RSP with result sucessfully registered is returned.



(define m3ua-sgp-rkm-v-002 m3ua-sgp-rkm-v-001)
;;; (m3ua-sgp-rkm-v-002 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if a REG_RSP with result sucessfully registered is returned.



(define (m3ua-sgp-rkm-v-003 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 1)
				     (m3ua-make-destination-point-code-parameter tester-pc))))))
    (let ((rc (m3ua-get-routing-context-from-reg-rsp (m3ua-wait-for-message fd m3ua-reg-rsp-message?))))
      (m3ua-send-message fd 0 (m3ua-make-dereg-req-message
			       (list (m3ua-make-routing-context-parameter (list rc))))))
    (m3ua-wait-for-message fd m3ua-dereg-rsp-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-sgp-rkm-v-003 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if a DEREG_RSP with result sucessfully deregistered is returned.



(define (m3ua-sgp-rkm-v-004 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 1)
				     (m3ua-make-destination-point-code-parameter tester-pc))))))
    (m3ua-wait-for-message fd m3ua-error-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-sgp-rkm-v-004 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an ERROR(Unsupported Message Class) is returned.
;;; FIXME: Other error codes should be also OK.


(define (m3ua-sgp-rkm-i-003 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 1))))))
    (m3ua-wait-for-message fd m3ua-reg-rsp-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-sgp-rkm-i-003 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an REG_RSP with result ERROR - Invalid routing key is returned.
;;; FIXME: Is this really an invalid RC? At least it does not make sense...



(define (m3ua-sgp-rkm-i-004 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 1)
				     (m3ua-make-destination-point-code-parameter tester-invalid-pc))))))
    (m3ua-wait-for-message fd m3ua-reg-rsp-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-sgp-rkm-i-004 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an REG_RSP with result ERROR - Invalid DPC is returned.



(define (m3ua-sgp-rkm-i-005 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 1)
				     (m3ua-make-destination-point-code-parameter tester-pc)
				     (m3ua-make-network-appearance-parameter invalid-network-appearance))))))
    (m3ua-wait-for-message fd m3ua-reg-rsp-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-sgp-rkm-i-005 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an REG_RSP with result ERROR - Invalid Network Appearance is returned.



(define (m3ua-sgp-rkm-i-006 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 1)
				     (m3ua-make-destination-point-code-parameter tester-pc))))))
    (m3ua-wait-for-message fd m3ua-reg-rsp-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 2)
				     (m3ua-make-destination-point-code-parameter tester-pc))))))
    (m3ua-wait-for-message fd m3ua-reg-rsp-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-sgp-rkm-i-006 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an REG_RSP with result ERROR - Can not support unique routing key is returned.



(define (m3ua-sgp-rkm-i-007 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 1)
				     (m3ua-make-destination-point-code-parameter tester-unauthorized-pc))))))
    (m3ua-wait-for-message fd m3ua-reg-rsp-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-sgp-rkm-i-007 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an REG_RSP with result ERROR - Error Permission Denied is returned.



(define (m3ua-sgp-rkm-i-008 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 1)
				     (m3ua-make-destination-point-code-parameter tester-unprovisioned-pc))))))
    (m3ua-wait-for-message fd m3ua-reg-rsp-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-sgp-rkm-i-008 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an REG_RSP with result ERROR - Error Routing key not currently provsioned is returned.



(define (m3ua-sgp-rkm-i-009 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 1)
				     (m3ua-make-destination-point-code-parameter tester-pc))))))
    (m3ua-wait-for-message fd m3ua-reg-rsp-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-sgp-rkm-i-009 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an REG_RSP with result ERROR - Error Insufficient Resources is returned.
;;; FIXME: How to arrange that the SUT is out of resources



(define (m3ua-sgp-rkm-i-010 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 1)
				     (m3ua-make-destination-point-code-parameter tester-pc)
				     (m3ua-make-circuit-range-parameter (list (list tester-pc 0 0))))))))
    (m3ua-wait-for-message fd m3ua-reg-rsp-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-sgp-rkm-i-010 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an REG_RSP with result ERROR - Error Unsupported RK Parameter Field is returned.
;;; It is assumed that the SUT does not support the circuit range parameter...



(define (m3ua-sgp-rkm-i-traffic-mode-test tester-addr tester-port sut-addr sut-port traffic-mode-type-1 traffic-mode-type-2)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 1)
				     (m3ua-make-traffic-mode-type-parameter traffic-mode-type-1)
				     (m3ua-make-destination-point-code-parameter tester-pc))))))
    (m3ua-wait-for-message fd m3ua-reg-rsp-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 2)
				     (m3ua-make-traffic-mode-type-parameter traffic-mode-type-2)
				     (m3ua-make-destination-point-code-parameter tester-pc))))))
    (m3ua-wait-for-message fd m3ua-reg-rsp-message?)
    (sleep 1)
    (close fd)))



(define (m3ua-sgp-rkm-i-011  tester-addr tester-port sut-addr sut-port)
  (m3ua-sgp-rkm-i-traffic-mode-test tester-addr tester-port sut-addr sut-port m3ua-traffic-mode-type-override m3ua-traffic-mode-type-loadshare))
;;; (m3ua-sgp-rkm-i-011 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an REG_RSP with result ERROR - Error Unsupported/Invalid Traffic Handling Mode is returned.



(define (m3ua-sgp-rkm-i-012  tester-addr tester-port sut-addr sut-port)
  (m3ua-sgp-rkm-i-traffic-mode-test tester-addr tester-port sut-addr sut-port m3ua-traffic-mode-type-override m3ua-traffic-mode-type-broadcast))
;;; (m3ua-sgp-rkm-i-012 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an REG_RSP with result ERROR - Error Unsupported/Invalid Traffic Handling Mode is returned.



(define (m3ua-sgp-rkm-i-013 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 1)
				     (m3ua-make-traffic-mode-type-parameter m3ua-traffic-mode-type-invalid)
				     (m3ua-make-destination-point-code-parameter tester-pc))))))
    (m3ua-wait-for-message fd m3ua-reg-rsp-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-sgp-rkm-i-013 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an REG_RSP with result ERROR - Error Unsupported/Invalid Traffic Handling Mode is returned.



(define (m3ua-sgp-rkm-i-014  tester-addr tester-port sut-addr sut-port)
  (m3ua-sgp-rkm-i-traffic-mode-test tester-addr tester-port sut-addr sut-port m3ua-traffic-mode-type-loadshare m3ua-traffic-mode-type-override))
;;; (m3ua-sgp-rkm-i-014 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an REG_RSP with result ERROR - Error Unsupported/Invalid Traffic Handling Mode is returned.



(define (m3ua-sgp-rkm-i-015  tester-addr tester-port sut-addr sut-port)
  (m3ua-sgp-rkm-i-traffic-mode-test tester-addr tester-port sut-addr sut-port m3ua-traffic-mode-type-loadshare m3ua-traffic-mode-type-broadcast))
;;; (m3ua-sgp-rkm-i-015 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an REG_RSP with result ERROR - Error Unsupported/Invalid Traffic Handling Mode is returned.



(define (m3ua-sgp-rkm-i-017  tester-addr tester-port sut-addr sut-port)
  (m3ua-sgp-rkm-i-traffic-mode-test tester-addr tester-port sut-addr sut-port m3ua-traffic-mode-type-broadcast m3ua-traffic-mode-type-override))
;;; (m3ua-sgp-rkm-i-017 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an REG_RSP with result ERROR - Error Unsupported/Invalid Traffic Handling Mode is returned.



(define (m3ua-sgp-rkm-i-018  tester-addr tester-port sut-addr sut-port)
  (m3ua-sgp-rkm-i-traffic-mode-test tester-addr tester-port sut-addr sut-port m3ua-traffic-mode-type-broadcast m3ua-traffic-mode-type-loadshare))
;;; (m3ua-sgp-rkm-i-018 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an REG_RSP with result ERROR - Error Unsupported/Invalid Traffic Handling Mode is returned.



(define (m3ua-sgp-rkm-i-020 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 1)
				     (m3ua-make-destination-point-code-parameter tester-pc))))))
    (m3ua-send-message fd 0 (m3ua-make-dereg-req-message
			     (list (m3ua-make-routing-context-parameter (list tester-rc-invalid)))))
    (m3ua-wait-for-message fd m3ua-dereg-rsp-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-sgp-rkm-i-020 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an REG_RSP with result ERROR - Error Not Registered is returned.



(define (m3ua-sgp-rkm-i-021 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 1)
				     (m3ua-make-destination-point-code-parameter tester-pc))))))
    (let ((rc (m3ua-get-routing-context-from-reg-rsp (m3ua-wait-for-message fd m3ua-reg-rsp-message?))))
      (m3ua-send-message fd 0 (m3ua-make-dereg-req-message
			       (list (m3ua-make-routing-context-parameter (list rc))))))
    (m3ua-wait-for-message fd m3ua-dereg-rsp-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-sgp-rkm-i-021 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if a DEREG_RSP with result Error - Permission Denied is returned.
;;; FIXME: Please make sure that the registered routing key is not authorized for dereg.



(define (m3ua-sgp-rkm-i-022 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-reg-req-message
			     (list
			      (m3ua-make-routing-key-parameter
			       (list (m3ua-make-local-routing-key-identifier-parameter 1)
				     (m3ua-make-destination-point-code-parameter tester-pc))))))
    (let ((rc (m3ua-get-routing-context-from-reg-rsp (m3ua-wait-for-message fd m3ua-reg-rsp-message?))))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-message (list (m3ua-make-routing-context-parameter (list rc)))))
      (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
      (m3ua-wait-for-message fd m3ua-notify-message?)
      (m3ua-send-message fd 0 (m3ua-make-dereg-req-message
			       (list (m3ua-make-routing-context-parameter (list rc))))))
    (m3ua-wait-for-message fd m3ua-dereg-rsp-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-sgp-rkm-i-022 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if a DEREG_RSP with result Error - ASP Currently Active For Routing Context is returned.



(define (m3ua-sgp-rkm-i-023 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-wait-for-message fd m3ua-notify-message?)
    (m3ua-send-message fd 0 (m3ua-make-message m3ua-rkm-message-class m3ua-reserved-rkm-message-type (list)))
    (m3ua-wait-for-message fd m3ua-error-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-sgp-rkm-i-023 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if an ERROR (Unsuported Message Type) is returned.

(define (m3ua-sgp-ssnm-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-daud-message (list (m3ua-make-affected-point-code-parameter (list (list 0 tester-congested-pc))))))
    (m3ua-wait-for-message fd m3ua-scon-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-sgp-ssnm-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an SCON.

(define (m3ua-sgp-ssnm-002 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-daud-message (list (m3ua-make-affected-point-code-parameter (list (list 0 tester-restricted-pc))))))
    (m3ua-wait-for-message fd m3ua-drst-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-sgp-ssnm-002 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an DRST.

(define (m3ua-sgp-ssnm-003 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-daud-message (list (m3ua-make-affected-point-code-parameter (list (list 0 tester-unavailable-pc))))))
    (m3ua-wait-for-message fd m3ua-duna-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-sgp-ssnm-003 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an DUNA.

(define (m3ua-sgp-ssnm-004 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-daud-message (list (m3ua-make-affected-point-code-parameter (list (list 0 tester-available-pc))))))
    (m3ua-wait-for-message fd m3ua-dava-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-sgp-ssnm-004 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an DAVA.

(define (m3ua-sgp-ssnm-004 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-daud-message (list (m3ua-make-affected-point-code-parameter (list (list 255 tester-available-pc))))))
   (sleep 1)
    (close fd)
    m3ua-test-result-unknown))
;;; (m3ua-sgp-ssnm-004 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an DAVA.
