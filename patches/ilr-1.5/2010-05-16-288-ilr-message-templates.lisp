(in-package :registry)

(defpatch (ilr 1.5 288 message-templates) (&key delete)

  (if delete
      (mapcar #'drop-instance (get-instances-by-class 'message-template)))

  ;; Create message-template instances
  (values
   (make-instance 'message-template :subject "ILR Update Message" :body "<%author%> writes:

<%title%>
Date: <%date%>

<%content%>
" :event "new-blog-entry")
   (make-instance 'message-template :subject "[ILR] New comment" :body "You are an editor for an ILR survey that has a new comment on a question

Comment by <%author%> (<%date%>) for question
\"<%question%>\"

Comment text:

<%comment%>


<%context%>


" :event "new-comment")
   (make-instance 'message-template :subject "[ILR] New post for <%topic%>" :body "<%author%> writes:

<%content%>

<%original%>

Visit this topic \"<%topic%>\":
<%url%>

" :event "new-forum-post")
   (make-instance 'message-template :subject "[ILR] Forum digest for <%date%>" :body "<%count%> new messages

<%activity%>" :event "forum-activity-summary")
   (make-instance 'message-template :subject "ILR news and reminders" :body "<%news%>
=========================================
<%next-steps%>
=========================================
<%forum-activity%>" :event "site-update")
   ))
