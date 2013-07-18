;------------------------------------------------------------------------------
;tasker
;Copyright (c) 2013, Joshua Scoggins 
;All rights reserved.
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions are met:
;    * Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;    * Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;    * Neither the name of tasker nor the
;      names of its contributors may be used to endorse or promote products
;      derived from this software without specific prior written permission.
;
;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;DISCLAIMED. IN NO EVENT SHALL Joshua Scoggins BE LIABLE FOR ANY
;DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;------------------------------------------------------------------------------
; Task.clp - Represents something to do
;------------------------------------------------------------------------------
(load* Note.clp)
(defclass Task
  (is-a USER)
  (slot title
        (type LEXEME)
        (default ?NONE))
  (slot status
        (type SYMBOL)
        (allowed-symbols open closed postponed reopened))
  (slot note-count
        (type INTEGER)
        (range 0 ?VARIABLE))
  (multislot notes
             (type INSTANCE)
             (allowed-classes Note))
  (message-handler is primary)
  (message-handler close primary)
  (message-handler note# primary)
  (message-handler reopen primary)
  (message-handler postpone primary)
  (message-handler new-note primary)
  (message-handler note-id primary))

(defmessage-handler Task is primary
                    (?status)
                    (eq ?self:status ?status))

(defmessage-handler Task close primary
                    ()
                    (if (neq ?self:status closed) then
                      (bind ?self:status closed)))

(defmessage-handler Task note# primary
                    (?index)
                    (nth$ ?index ?self:notes))

(defmessage-handler Task reopen primary
                    ()
                    (if (eq ?self:status closed) then
                      (bind ?self:status reopened)))

(defmessage-handler Task postpone primary
                    ()
                    (if (not (neq ?self:status open reopened)) then
                      (bind ?self:status postponed)))

(defmessage-handler Task new-note primary
                    (?message)
                    (bind ?r (make-instance of Note
                                            (index (send ?self note-id))
                                            (parent (instance-name ?self))
                                            (message ?message)))
                    (slot-direct-insert$ notes 
                                         (+ ?self:note-count 2) 
                                         (instance-name ?r)))

(defmessage-handler Task note-id primary
                    ()
                    (bind ?nid ?self:note-index)
                    (bind ?self:note-index (+ ?nid 1))
                    (return ?nid))

