(in-package :registry)

#+nil
(progn
  (make-instance 'user 
		 :username "eslick"
		 :password (create-sha1-password "apple")
		 :first-name "Ian"
		 :last-name "Eslick"
		 :address (make-instance 'address
					 :street "3 Hancock Park #2"
					 :city "Cambridge"
					 :state "Massachusetts")))
#+nil
(progn
  (let ((user (get-instance-by-value 'user 'username "eslick")))
    (persist-objects *default-store*
      (list (make-instance 'article
			   :user user
			   :page "home"			 
			   :title "Welcome to LAMsight"
			   :content "This is a test paragraph for the home page content")
	    (make-instance 'article
			   :user user
			   :page "home"
			   :title "About LAM"
			   :content "This is another test paragraph of the home page content")))))


#+nil
(progn
  (let ((user (get-instance-by-value 'user 'username "eslick")))
    (let* ((cat (make-instance 'forum-category
			       :name "General"))
	   (topic (make-instance 'forum-topic
				 :category cat
				 :owner user
				 :tags nil
				 :subject "Welcome to LAMsight: the FAQ"
				 :content "This is a placeholder for FAQ content"
				 :slug "welcome-faq"
				 :related-content nil))
	   (post1 (make-instance 'forum-post
				 :owner user
				 :topic topic
				 :title "Missing FAQs"
				 :content "The FAQ doesn't expand and explain the legalese of the terms of use"))
	   (empty (progn (sleep 2) 1))
	   (post2 (make-instance 'forum-post
				 :owner user
				 :topic topic
				 :title "No, it's there"
				 :content "Not to be rude, but the FAQ does contain that in question #20")))
      (make-instance 'forum-post
		     :owner user
		     :topic topic
		     :title "Welcome to LAMsight"
		     :content "This site is dedicated to the...")
      (make-instance 'forum-post
		     :owner user
		     :topic topic
		     :title "Welcome to LAMsight"
		     :content "This site is dedicated to the...")
      (make-instance 'forum-post
		     :owner user
		     :topic topic
		     :title "Welcome to LAMsight"
		     :content "This site is dedicated to the...")
      (make-instance 'forum-post
		     :owner user
		     :topic topic
		     :title "Welcome to LAMsight"
		     :content "This site is dedicated to the...")
      (make-instance 'forum-post
		     :owner user
		     :topic topic
		     :title "Welcome to LAMsight"
		     :content "This site is dedicated to the...")
      (make-instance 'forum-post
		     :owner user
		     :topic topic
		     :title "Welcome to LAMsight"
		     :content "This site is dedicated to the..."))))

;; Questions

#+nil
(progn
  (let ((guide (make-instance 'guide
			      :title "Tour Guide"
			      :show-title-p nil)))
    (let ((chapters
	   (list
	    (make-instance 'guide-node
			   :title "About the tour"
			   :body "This is all about the tour")
	    (make-instance 'guide-node
			   :title "Exploring LAMsight"
			   :body "This is about exploring LAMsight"
			   :children (list
				      (make-instance 'guide-node
						     :title "Exploring the forums"
						     :body "About exploring the forums")
				      (make-instance 'guide-node
						     :title "Exporing surveys"
						     :body "About exploring surveys and diaries")
				      (make-instance 'guide-node
						     :title "Exploring investigations"
						     :body "About exploring investigations"))))))
	  (setf (guide-children guide) chapters))))



	   
