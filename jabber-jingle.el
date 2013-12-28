;;; jabber-jingle.el --- general support for Jingle (XEP-0166)  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;;;###autoload
(eval-after-load "jabber-disco"
  '(jabber-disco-advertise-feature "urn:xmpp:jingle:1"))

;;;###autoload
(eval-after-load "jabber-iq"
  '(add-to-list 'jabber-iq-set-xmlns-alist
		'("urn:xmpp:jingle:1" . jabber-jingle-process-iq)))

(defvar jabber-jingle-active-sessions ()
  "Alist mapping jid+sid combinations to active sessions.
The keys are (JID . SID), where JID is the symbol for the address
of the peer, and SID is the session id.  The values are FSMs.")

(defvar jabber-jingle-applications ()
  "Alist mapping XML namespaces to functions.
The functions are called on incoming Jingle session initiation
requests, and should accept the following arguments:

- JC, the Jabber connection being used
- SID, the session id given
- JID, the Jabber ID of the initiator
- OUR-ROLE, currently always :responder

The functions should return a newly started fsm instance.")

;;;###autoload
(defun jabber-jingle-process-iq (jc xml-data)
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
	 (jid-symbol (jabber-jid-symbol from))
	 (id (jabber-xml-get-attribute xml-data 'id))
	 (jingle (jabber-iq-query xml-data))
	 (action (jabber-xml-get-attribute jingle 'action))
	 (sid (jabber-xml-get-attribute jingle 'sid))
	 (session (cdr (assoc (cons jid-symbol sid) jabber-jingle-active-sessions))))
    (cond
     ((and (string= action "session-initiate")
	   session)
      (jabber-signal-error "modify" 'unexpected-request nil
			   '((out-of-order ((xmlns . "urn:xmpp:jingle:errors:1"))))))
     ((and (null session) (not (string= action "session-initiate")))
      (jabber-signal-error "modify" 'item-not-found nil
			   '((unknown-session ((xmlns . "urn:xmpp:jingle:errors:1"))))))
     ((string= action "session-initiate")
      ;; So this represents a valid attempt to initiate a session.
      (let* ((content (car (jabber-xml-get-children jingle 'content)))
	     (description (car (jabber-xml-get-children content 'description)))
	     (application-ns (jabber-xml-get-xmlns description))
	     (start-function (cdr (assoc application-ns jabber-jingle-applications))))
	(if (null start-function)
	    ;; If it turns out we don't understand what the other
	    ;; party is asking for, terminate the session.
	    (progn
	      ;; But we have to acknowledge the session-initiate first.
	      (jabber-send-iq jc from "result" nil nil nil nil nil id)
	      (jabber-send-iq jc from "set"
			      `(jingle ((xmlns . "urn:xmpp:jingle:1")
					(action . "session-terminate")
					(sid . ,sid))
				       (reason () (unsupported-applications)))
			      nil nil nil nil))
	  ;; We know what this request is about; start a state machine
	  ;; to deal with it.
	  (let ((new-fsm
		 (funcall start-function jc sid from :responder)))
	    (push (cons (cons jid-symbol sid) new-fsm) jabber-jingle-active-sessions)
	    (jabber-send-iq jc from "result" nil nil nil nil nil id)
	    (fsm-send new-fsm `(:session-initiate ,content))))))
     (t
      (fsm-send session `(:jingle xml-data))))))

(provide 'jabber-jingle)
;;; jabber-jingle.el ends here
