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
; Frontend.clp - Contains functions to streamline the process of using tasks 
;------------------------------------------------------------------------------
(batch* "Task.clp")
;------------------------------------------------------------------------------
(defgeneric tasker:new)
(defgeneric tasker:close-task)
(defgeneric tasker:reopen-task)
(defgeneric tasker:postpone-task)
(defgeneric tasker:new-note)
(defgeneric tasker:new-task)
(defgeneric tasker:tasks)
(defgeneric tasker:closed-tasks)
(defgeneric tasker:postponed-tasks)
(defgeneric tasker:save-tasks)
(defgeneric tasker:load-tasks)
(defgeneric tasker:read-task)
;------------------------------------------------------------------------------
(defmethod tasker:new 
  ((?type SYMBOL (eq ?type task))
   (?title LEXEME)
   (?description LEXEME)
   (?priority NUMBER))
  (tasker:new-task ?title ?description ?priority))

(defmethod tasker:new 
  ((?type SYMBOL (eq ?type task))
   (?title LEXEME)
   (?description LEXEME))
  (tasker:new-task ?title ?description))

(defmethod tasker:new 
  ((?type SYMBOL (eq ?type note))
   (?parent INSTANCE Task)
   (?description LEXEME))
  (tasker:new-note ?parent ?description))
;------------------------------------------------------------------------------

(defmethod tasker:new-task
  ((?title LEXEME)
   (?description LEXEME)
   (?priority NUMBER))
  (tasker:new-note 
    (make-instance of Task
                   (title ?title)
                   (priority ?priority)) 
    ?description))

(defmethod tasker:new-task
  ((?title LEXEME)
   (?description LEXEME))
  (tasker:new-task ?title ?description 0))

;------------------------------------------------------------------------------
(defmethod tasker:new-note
  ((?parent INSTANCE Task) 
   (?description LEXEME))
  (send ?parent new-note ?description))

(defmethod tasker:new-note
 ((?parent SYMBOL)
  (?description LEXEME))
 (tasker:new-note (symbol-to-instance-name ?parent) ?description))
;------------------------------------------------------------------------------
(defmethod tasker:close-task
  ((?task INSTANCE Task)
   (?reason LEXEME))
  (send ?task close ?reason))

(defmethod tasker:close-task
  ((?task SYMBOL)
   (?reason LEXEME))
  (tasker:close-task (symbol-to-instance-name ?task) ?reason))
;------------------------------------------------------------------------------
(defmethod tasker:reopen-task
  ((?task INSTANCE Task)
   (?reason LEXEME))
  (send ?task reopen ?reason))

(defmethod tasker:reopen-task
  ((?task SYMBOL)
   (?reason LEXEME))
  (tasker:reopen-task (symbol-to-instance-name ?task) ?reason))

;------------------------------------------------------------------------------
(defmethod tasker:postpone-task
  ((?task INSTANCE Task)
   (?reason LEXEME))
  (send ?task postpone ?reason))

(defmethod tasker:postpone-task
  ((?task SYMBOL)
   (?reason LEXEME))
  (tasker:postpone-task (symbol-to-instance-name ?task) ?reason))
;------------------------------------------------------------------------------
(defmethod tasker:tasks
  ()
  (do-for-all-instances 
    ((?z Task)) 
    (not (neq (send ?z get-status) open reopened))
    (printout t (format nil "%s - %s" 
                      (instance-name-to-symbol (instance-name ?z))
                      (send ?z get-title)) crlf)))
;------------------------------------------------------------------------------
(defmethod tasker:closed-tasks
  ()
  (printout t "Closed Tasks: " crlf)
  (do-for-all-instances 
    ((?z Task)) 
    (not (neq (send ?z get-status) closed))
    (printout t (format nil "  %s - %s" 
                        (instance-name-to-symbol (instance-name ?z))
                        (send ?z get-title)) crlf)))
;------------------------------------------------------------------------------
(defmethod tasker:postponed-tasks
  ()
  (printout t "Postponed Tasks: " crlf)
  (do-for-all-instances 
    ((?z Task)) 
    (not (neq (send ?z get-status) postponed))
    (printout t (format nil "  %s - %s" 
                        (instance-name-to-symbol (instance-name ?z))
                        (send ?z get-title)) crlf)))

;------------------------------------------------------------------------------
(defmethod tasker:save-tasks
 ((?path LEXEME))
 (save-instances ?path visible))

(defmethod tasker:load-tasks
 ((?path LEXEME))
 (load-instances ?path))
;------------------------------------------------------------------------------
(defmethod tasker:read-task
 ((?task INSTANCE Task))
 (printout t (send ?task get-title) crlf)
 (progn$ (?note (send ?task get-notes)) 
         (printout t (send ?note get-message) crlf)))

(defmethod tasker:read-task
 ((?task SYMBOL))
 (tasker:read-task (symbol-to-instance-name ?task)))
