;;; 
;;; Copyright (C) 2005 M. Tuexen tuexen@fh-muenster.de
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

;;; $Id: m3ua-asp-tests.scm,v 1.12 2012/08/28 19:56:13 tuexen Exp $

;;; History
;;; 13.09.2005: Implement ASP tests.
;;; 09.10.2005: Provide example calls for the ASP.
;;; 07.01.2006: Implement missing ASP tests.
;;; 27.08.2006: Added m3ua-asp-aspsm-v-005-alternate
;;; 27.08.2006: Added m3ua-asp-aspsm-i-002-alternate
;;;
;;; Definition of the tests for the ASP
;;;


(define (m3ua-asp-aspsm-v-002 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (let ((msg (m3ua-wait-for-message fd m3ua-asp-up-message?)))
      (close fd)
      (if (= (m3ua-get-version msg) 1)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-asp-aspsm-v-002 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the version in the common header of the
;;; received packet is 1.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive))


(define (m3ua-asp-aspsm-v-005 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((msg (m3ua-wait-for-message fd m3ua-asp-down-message?)))
      (close fd)
      (if (= (m3ua-get-version msg) 1)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-asp-aspsm-v-005 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the version in the common header of the
;;; received packet is 1.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-down))


(define (m3ua-asp-aspsm-v-005-alternate tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active))))
    (let ((asp-inactive (m3ua-wait-for-message fd m3ua-asp-inactive-message?)))
      (if (= (m3ua-get-version asp-inactive) 1)
	  (begin
	    (m3ua-send-message fd 0 (m3ua-make-asp-inactive-ack-message (m3ua-get-parameters asp-inactive)))
	    (m3ua-wait-for-message fd m3ua-asp-down-message?)
	    (m3ua-send-message fd 0 (m3ua-make-asp-down-ack-message))
	    (close fd)
	    m3ua-test-result-passed)
	  (begin 
	    (close fd)
	    m3ua-test-result-failed)))))
;;; (m3ua-asp-aspsm-v-005-alternate tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ASP_INACTIVE with version 1.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active m3ua-asp-inactive))


(define (m3ua-asp-aspsm-i-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-common-header (+ m3ua-version 1)
						     m3ua-reserved
						     m3ua-aspsm-message-class
						     m3ua-aspup-ack-message-type
						     m3ua-common-header-length))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-invalid-version-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-asp-aspsm-i-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT returns an ERROR(invalid version)
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive))


(define (m3ua-asp-aspsm-i-002 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (m3ua-wait-for-message fd m3ua-asp-down-message?)
    (m3ua-send-message fd 0 (m3ua-make-common-header (+ m3ua-version 1)
						     m3ua-reserved
						     m3ua-aspsm-message-class
						     m3ua-aspdn-ack-message-type
						     m3ua-common-header-length))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-invalid-version-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-asp-aspsm-i-002 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT returns an ERROR(invalid version)
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-down))


(define (m3ua-asp-aspsm-i-002-alternate tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active))))
    (let ((asp-inactive (m3ua-wait-for-message fd m3ua-asp-inactive-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-inactive-ack-message (m3ua-get-parameters asp-inactive))))
    (m3ua-wait-for-message fd m3ua-asp-down-message?)
    (m3ua-send-message fd 0 (m3ua-make-common-header (+ m3ua-version 1)
						     m3ua-reserved
						     m3ua-aspsm-message-class
						     m3ua-aspdn-ack-message-type
						     m3ua-common-header-length)) 
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-invalid-version-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-asp-aspsm-i-002-alternate tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ASP_INACTIVE with version 1.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active m3ua-asp-inactive m3ua-asp-down))


(define (m3ua-asp-aspsm-i-003 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
;;  FIXME: Should I send the ASPUP-ACK?
;;  (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (m3ua-send-message fd 0 (m3ua-make-message m3ua-aspsm-message-class
					       m3ua-reserved-aspsm-message-type
					       (list)))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-unsupported-message-type-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-asp-aspsm-i-003 tester-addr tester-port sut-addr sut-port)
;;; FIXME: Why states the ETSI document that the ASP is marked as ASP_INACTIVE
;;; This test is passed iff the SUT returns an ERROR(unsupported message type)
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive))


(define (m3ua-asp-aspsm-o-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-down-ack-message))
    (let ((msg (m3ua-wait-for-message-with-timeout fd m3ua-asp-active-message? 2)))
      (close fd)
      (if (null? msg)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-asp-aspsm-o-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT does not send an ASP_ACTIVE. FIXME.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive))


(define (m3ua-asp-aspsm-o-002 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message asp-active-ack-message-parameters))
    (let ((msg (m3ua-recv-message-with-timeout fd 2)))
      (close fd)
      (if (or (null? msg)
	      (and (m3ua-error-message? msg)
		   (= (m3ua-get-error-code-from-message msg) m3ua-unexpected-message-error-code))
	      (m3ua-asp-up-message? msg))
	  m3ua-test-result-passed
	  (if (m3ua-data-message? msg)
	      m3ua-test-result-failed
	      m3ua-test-result-unknown)))))
;;; (m3ua-asp-aspsm-o-002 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT does send an ERROR(unexpected message).
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive))


(define (m3ua-asp-asptm-v-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active))))
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-asp-asptm-v-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ASP_ACTIVE.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active))


(define (m3ua-asp-asptm-v-002 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (if (= (m3ua-get-version asp-active) 1)
	  (begin
	    (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active)))
	    (close fd)
	    m3ua-test-result-passed)
	  (begin 
	    (close fd)
	    m3ua-test-result-failed)))))
;;; (m3ua-asp-asptm-v-002 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ASP_ACTIVE with version 1.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active))


(define (m3ua-asp-asptm-v-005 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active))))
    (let ((asp-inactive (m3ua-wait-for-message fd m3ua-asp-inactive-message?)))
      (if (= (m3ua-get-version asp-inactive) 1)
	  (begin
	    (m3ua-send-message fd 0 (m3ua-make-asp-inactive-ack-message (m3ua-get-parameters asp-inactive)))
	    (close fd)
	    m3ua-test-result-passed)
	  (begin 
	    (close fd)
	    m3ua-test-result-failed)))))
;;; (m3ua-asp-asptm-v-005 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ASP_INACTIVE with version 1.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active m3ua-asp-inactive))


(define (m3ua-asp-asptm-v-007 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?))
	  (heartbeat-data (random-bytes 5000)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active)))
      (m3ua-send-message fd 0 (m3ua-make-beat-message heartbeat-data))
      (let ((m (m3ua-wait-for-message fd (lambda (m) (or (m3ua-beat-ack-message? m)
							 (m3ua-error-message? m))))))
	(close fd)
	(if (m3ua-beat-ack-message? m)
	    m3ua-test-result-passed
	    m3ua-test-result-failed)))))
;;; (m3ua-asp-asptm-v-007 tester-addr tester-port sut-addr sut-port)
;;; The last parameter is the length the hearbeat data.
;;; This test is passed iff the SUT sends a BEAT_ACK.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active m3ua-asp-reflect-beat))


(define (m3ua-asp-asptm-v-008 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?))
	  (heartbeat-data (random-bytes 600)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active)))
      (m3ua-send-message fd 0 (m3ua-make-beat-message heartbeat-data))
      (let ((m (m3ua-wait-for-message fd (lambda (m) (or (m3ua-beat-ack-message? m)
							 (m3ua-error-message? m))))))
	(close fd)
	(if (and (m3ua-beat-ack-message? m)
		 (equal? (m3ua-make-beat-ack-message heartbeat-data) m))
	    m3ua-test-result-passed
	    m3ua-test-result-failed)))))
;;; (m3ua-asp-asptm-v-008 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends a BEAT_ACK with unchanged data.
;;; This is indicated by returning true.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active m3ua-asp-reflect-beat))


(define (m3ua-asp-asptm-i-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-increment-version 
			       (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active))))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-invalid-version-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed)))))
;;; (m3ua-asp-asptm-i-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ERROR(invalid version).
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active))


(define (m3ua-asp-asptm-i-002 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active)))
      (m3ua-wait-for-message fd m3ua-asp-inactive-message?)
      (m3ua-send-message fd 0 (m3ua-increment-version 
			       (m3ua-make-asp-inactive-ack-message (m3ua-get-parameters asp-active))))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-invalid-version-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed)))))
;;; (m3ua-asp-asptm-i-002 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ERROR(invalid version).
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active m3ua-asp-inactive))


(define (m3ua-asp-asptm-i-003 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-message m3ua-asptm-message-class
						 m3ua-reserved-asptm-message-type
						 (list)))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-unsupported-message-type-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed)))))
;;; (m3ua-asp-asptm-i-003 tester-addr tester-port sut-addr sut-port)
;;; FIXME: Why does the ETSI doucment state that the IUT is in ASP_DOWN.
;;; This test is passed iff the SUT sends an ERROR(unsupported message type).
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active))


(define (m3ua-asp-asptm-o-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (m3ua-wait-for-message fd m3ua-asp-active-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((msg (m3ua-recv-message-with-timeout fd 2)))
      (close fd)
      (if (or (null? msg)
	      (and (m3ua-error-message? msg)
		   (= (m3ua-get-error-code-from-message msg) m3ua-unexpected-message-error-code))
	      (m3ua-asp-active-message? msg))
	  m3ua-test-result-passed
	  (if (m3ua-data-message? msg)
	      m3ua-test-result-failed
	      m3ua-test-result-unknown)))))
;;; (m3ua-asp-asptm-o-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ERROR(unexpected message).
;;; FIXME: How to test the data sending?
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active))


(define (m3ua-asp-mtr-v-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active)))
      (let ((m (m3ua-wait-for-message fd (lambda (m) (or (m3ua-data-message? m)
							 (m3ua-daud-message? m))))))
	(if (m3ua-daud-message? m)
	    (begin
	      (m3ua-send-message fd 0 (m3ua-make-dava-message (m3ua-get-parameters m)))
	      (m3ua-wait-for-message fd m3ua-data-message?))))
      (close fd)
      m3ua-test-result-unknown)))
;;; (m3ua-asp-mtr-v-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends DATA including a RC.
;;; FIXME
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active m3ua-asp-send-data))


(define (m3ua-asp-mtr-v-002 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active)))
      (let ((m (m3ua-wait-for-message fd (lambda (m) (or (m3ua-data-message? m)
							 (m3ua-daud-message? m))))))
	(if (m3ua-daud-message? m)
	    (begin
	      (m3ua-send-message fd 0 (m3ua-make-dava-message (m3ua-get-parameters m)))
	      (m3ua-wait-for-message fd m3ua-data-message?))))
      (close fd)
      m3ua-test-result-unknown)))
;;; (m3ua-asp-mtr-v-002 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends DATA including data.
;;; FIXME
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active m3ua-asp-send-data))


(define (m3ua-asp-mtr-v-003 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active)))
      (let ((m (m3ua-wait-for-message fd (lambda (m) (or (m3ua-data-message? m)
							 (m3ua-daud-message? m))))))
	(if (m3ua-daud-message? m)
	    (begin
	      (m3ua-send-message fd 0 (m3ua-make-dava-message (m3ua-get-parameters m)))
	      (m3ua-wait-for-message fd m3ua-data-message?))))
      (close fd)
      m3ua-test-result-unknown)))
;;; (m3ua-asp-mtr-v-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends DATA in a valid stream .
;;; FIXME
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active m3ua-asp-send-data))


(define (m3ua-asp-mtr-i-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active)))
      (m3ua-send-message fd 0 (m3ua-increment-version
			       (m3ua-make-data-message 0 0 0 0 0 0 (list) (list))))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-invalid-version-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed)))))
;;; (m3ua-asp-mtr-i-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ERROR(invalid version).
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active m3ua-asp-receive-data))


(define (m3ua-asp-mtr-i-002 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active)))
      (m3ua-send-message fd 0 (m3ua-make-message m3ua-reserved-message-class
						 0
						 (list)))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-unsupported-message-class-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed)))))
;;; (m3ua-asp-mtr-i-002 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ERROR(unsupported message class).
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active m3ua-asp-receive-data))


(define (m3ua-asp-mtr-i-003 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active)))
      (m3ua-send-message fd 0 (m3ua-make-message m3ua-tfer-message-class
						 m3ua-reserved-tfer-message-type
						 (list)))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-unsupported-message-type-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed)))))
;;; (m3ua-asp-mtr-i-003 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ERROR(unsupported message type).
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active m3ua-asp-receive-data))


(define (m3ua-asp-rkm-v-002 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((message (m3ua-wait-for-message fd m3ua-reg-req-message?)))
      (m3ua-send-message fd 0 (m3ua-make-reg-rsp-from-reg-req message)))
    (sleep 1)
    (close fd)))
;;; (m3ua-asp-rkm-v-002 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends a valid routing key.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-send-reg-req))


(define (m3ua-asp-rkm-v-003 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((reg-req (m3ua-wait-for-message fd m3ua-reg-req-message?)))
      (m3ua-send-message fd 0 (m3ua-make-reg-rsp-from-reg-req reg-req))
      (let ((dereg-req (m3ua-wait-for-message fd m3ua-dereg-req-message?)))
	(m3ua-send-message fd 0 (m3ua-make-dereg-rsp-from-dereg-req dereg-req))))
    (sleep 1)
    (close fd)))
;;; (m3ua-asp-rkm-v-003 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends a deregistration request.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-send-reg-req m3ua-asp-send-dereg-req))


(define (m3ua-asp-rkm-v-004 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((reg-req (m3ua-wait-for-message fd m3ua-reg-req-message?)))
      (m3ua-send-message fd 0 (m3ua-make-reg-rsp-from-reg-req reg-req))
      (let ((dereg-req (m3ua-wait-for-message fd m3ua-dereg-req-message?)))
	(m3ua-send-message fd 0 (m3ua-make-dereg-rsp-from-dereg-req dereg-req))))
    (sleep 1)
    (close fd)))
;;; (m3ua-asp-rkm-v-004 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends a deregistration request with correct routing context.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-send-reg-req m3ua-asp-send-dereg-req))


(define (m3ua-asp-rkm-i-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (m3ua-wait-for-message fd m3ua-reg-req-message?)
    (m3ua-send-message fd 0 (m3ua-make-message m3ua-rkm-message-class
					       m3ua-reserved-rkm-message-type
					       (list)))
    (m3ua-wait-for-message fd m3ua-error-message?)
    (sleep 1)
    (close fd)))
;;; (m3ua-asp-rkm-i-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ERROR(unsupported message type).
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-send-reg-req m3ua-asp-active))


(define (m3ua-asp-ssnm-001 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active))))
    (m3ua-send-message fd 0 (m3ua-make-duna-message (list (m3ua-make-affected-point-code-parameter (list (list 0 tester-unavailable-pc))))))
    (m3ua-wait-for-message fd m3ua-daud-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-asp-ssnm-001 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an DAUD.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active))

(define (m3ua-asp-ssnm-002 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active))))
    (m3ua-send-message fd 0 (m3ua-make-duna-message (list (m3ua-make-affected-point-code-parameter (list (list 255 tester-unavailable-pc))))))
    (m3ua-wait-for-message fd m3ua-daud-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-asp-ssnm-002 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an DAUD.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active))

(define (m3ua-asp-ssnm-003 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active))))
    (m3ua-send-message fd 0 (m3ua-make-drst-message (list (m3ua-make-affected-point-code-parameter (list (list 0 tester-restricted-pc))))))
    (m3ua-wait-for-message fd m3ua-daud-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-asp-ssnm-003 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an DAUD.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active))

(define (m3ua-asp-ssnm-004 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active))))
    (m3ua-send-message fd 0 (m3ua-make-drst-message (list (m3ua-make-affected-point-code-parameter (list (list 255 tester-restricted-pc))))))
    (m3ua-wait-for-message fd m3ua-daud-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-asp-ssnm-004 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an DAUD.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active))

(define (m3ua-asp-ssnm-005 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active))))
    (m3ua-send-message fd 0 (m3ua-make-scon-message (list (m3ua-make-affected-point-code-parameter (list (list 0 tester-congested-pc))))))
    (m3ua-wait-for-message fd m3ua-daud-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-asp-ssnm-005 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an DAUD.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active))

(define (m3ua-asp-ssnm-006 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active))))
    (m3ua-send-message fd 0 (m3ua-make-scon-message (list (m3ua-make-affected-point-code-parameter (list (list 255 tester-congested-pc))))))
    (m3ua-wait-for-message fd m3ua-daud-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-asp-ssnm-006 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an DAUD.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active))

(define (m3ua-asp-ssnm-007 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active))))
    (m3ua-send-message fd 0 (m3ua-make-dupu-message (list (m3ua-make-affected-point-code-parameter (list (list 0 tester-congested-pc)))
							  (m3ua-make-user-cause-parameter m3ua-mtp-user-isup m3ua-unequipped-remote-user-cause))))
    (m3ua-wait-for-message fd m3ua-daud-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-asp-ssnm-006 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an DAUD.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active))

(define (m3ua-asp-ssnm-008 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-accept tester-addr tester-port)))
    (m3ua-wait-for-message fd m3ua-asp-up-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-up-ack-message))
    (let ((asp-active (m3ua-wait-for-message fd m3ua-asp-active-message?)))
      (m3ua-send-message fd 0 (m3ua-make-asp-active-ack-message (m3ua-get-parameters asp-active))))
    (m3ua-send-message fd 0 (m3ua-make-dupu-message (list (m3ua-make-affected-point-code-parameter (list (list 255 tester-congested-pc)))
							  (m3ua-make-user-cause-parameter m3ua-mtp-user-isup m3ua-unequipped-remote-user-cause))))
    (m3ua-wait-for-message fd m3ua-error-message?)
    (close fd)
    m3ua-test-result-passed))
;;; (m3ua-asp-ssnm-006 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ERROR.
;;; (m3ua-run-asp tester-addr (list m3ua-asp-inactive m3ua-asp-active))
