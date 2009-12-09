//-- SoapBlox JavaScript
//-- 7/30/2007 (C)

	var req;
	var which;
	var debug = false;
	var baseUrl = "";
	var commentId = "";
	var diaryId = "";
	var previewTitle = "";
	var previewText = "";
	var postingComment = false;
	var canPost = false;
	var previewingComment = false;
	var hasError = false;
	var buttonState;

	// makes a comment appear when someone clicks "post a comment" or "reply"
	// id - the commentId (0 if "root"), dId - diaryId
	function makeCommentAppear(id, dId) {

		var tempTitle = "";
		var tempText = "";
	
		// if this isn't empty, we were trying to perviously post a comment
		// therefore, we need to clean out all the html in the previous spot
		// so we don't create "nodeLists" for postCommentTextArea and Title
		if (commentId != "") {
			// get the current "postcommenter" element
			var oldElem = document.getElementById('postcomment' + commentId);
	
			if (oldElem) {
				// this will store whatever we might have had before
				if (document.forms['rateForm'].elements['postCommentTitle']) {
					tempTitle = document.forms['rateForm'].elements['postCommentTitle'].value;
				}	
				if (document.forms['rateForm'].elements['postCommentTextArea']) {
					tempText = document.forms['rateForm'].elements['postCommentTextArea'].value;
				}
			
				// clear out the old HTML, so we don't create nodeLists
				oldElem.innerHTML = "";
			}
		}
	
		hideArea("post");

		// store the values globally for what we're currently looking at
	    commentId = id;
	    diaryId = dId;
	    
	    var elementName = 'postcomment' + id;
	    
	    // make sure the element is hidden
		Element.hide(elementName);

		// set the data in it		
	    document.getElementById(elementName).innerHTML = document.getElementById("contentHolder").innerHTML;
		document.forms['rateForm'].elements['postCommentTitle'].value = tempTitle;
		document.forms['rateForm'].elements['postCommentTextArea'].value = tempText;
	
		// make it slide down, take half a second, scaleFrom 3 to avoid "stutter" in IE
		Effect.SlideDown(elementName, { duration: 0.5, scaleFrom: 3});
		window.setTimeout("document.forms['rateForm'].elements['postCommentTitle'].focus()", 501);
		
	    postingComment = true;
	    // since we're starting a new preview cycle, we're going to need
	    // to use the slide down effect.  This ensures that
	    previewingComment = false;

	    var objDiv = document.getElementById(elementName);
		objDiv.scrollTop = objDiv.scrollHeight;
	}

	// what we do when someone hits cancel
	function cancelPost() {

		var cTextElem = document.forms['rateForm'].elements['postCommentTextArea'];
		var cTitleElem = document.forms['rateForm'].elements['postCommentTitle'];
		
		if ((cTextElem && cTextElem.value != "")
				|| (cTitleElem && cTitleElem.value != ""))
		{
			// if the don't want to cancel--stop.
			if (!confirm("Cancel this comment?")) {
				return;
			}
		}

		hideArea("cancel");
		// we no longer clear this out so in makeCommentAppear we can see if we have to clean
		// out some hidden HTML
		//commentId = "";
		diaryId = "";
		previewText = "";
		previewTitle = "";
		cTextElem.value = "";
		cTitleElem.value = "";
		previewingComment = false;	
		postingComment = false;
	}
	
	// this is the safari default button hack, where we use onfocus to determine what mode we're in
	function setButtonState(theValue) {
		buttonState = theValue;
	}

	// it's how we post a comment!
	function submitForm(theForm) {
	
		// alert(buttonState + " " + previewingComment);
	
		// stop the "enter in the subject" bug
		if (buttonState == "post" && !previewingComment
		 		|| buttonState == "post" && previewingComment && !canPost) {
			previewComment(theForm);
			return false;
		}
		

		var theForm = document.forms['rateForm'];	
		theForm.action = "postComment.do";
		theForm.elements['replyParentCommentId'].value = commentId;

		if (commentId == 0) {
			theForm.elements['diaryId'].value = diaryId;	
		}
		else {
			theForm.elements['commentId'].value = commentId;	

		}
		
		theForm.elements['replyDiaryId'].value = diaryId;	
		theForm.elements['submitType'].value = "post";	
		
		// alert("yo" + theForm.elements['submitType']);
		theForm.method = "POST";
		theForm.submit();
	}

	// trims a string
	function trim(str) {     
		if(!str || typeof str != 'string') {
			return null;     
		}
			
		return str.replace(/^[\s]+/,'').replace(/[\s]+$/,'').replace(/[\s]{2,}/,' '); 
		
	}

	function previewComment(theForm) {
	
		// if (tinyMCE) {
			// tinyMCE.triggerSave();
		// }

		var url = "ajaxPreviewComment.do";
		var errorDiv = "errorcomment" + commentId;
		
		// hide any error we might have
		if (hasError) {
			Element.hide(errorDiv);
		}
			
		var cTitleElem = document.forms['rateForm'].elements["postCommentTitle"];
		var cTextElem =  document.forms['rateForm'].elements["postCommentTextArea"];
	
		
		previewTitle = cTitleElem.value;
		previewText = cTextElem.value;
	
	    // trim it so we remove the white sapces
	    previewTitle = trim(previewTitle);

	    // if we have nothing, set the error
	    if (previewTitle == null || previewTitle == "") {
			Effect.Appear(errorDiv, { duration: 0.5 });
			disallowPost();
			hasError = true;
	    	return;
	    }

	    // if we get this far we don't have an error!
	    hasError = false;
	    
	    // apparently using "escape" funcation messes with utf-8 so we just explicity
	    // replace newlines with their url-encoding equavilent.
	    previewText = escape(previewText); 
	    
	    previewText = previewText.replace(/\%u201C/ig, "%22");
	    previewText = previewText.replace(/\%u201D/ig, "%22");
  	    previewText = previewText.replace(/\%u2018/ig, "%27");
  	    previewText = previewText.replace(/\%u2019/ig, "%27");  	    
  	    previewText = previewText.replace(/\%u2013/ig, "-");  
  	    previewText = previewText.replace(/\%u2014/ig, "-");    	      	    
  	    previewText = previewText.replace(/\%u2026/ig, "...");    	
  	    previewText = previewText.replace(/\+/g, "%2B");
	    
	    //previewText.replace(/\n/g, "%0a");
//	    previewText = previewText.replace(/\+/g, "%2b");
  // 	    previewText = previewText.replace(/\%/g, "%25");
//   	    alert(previewText);
	    
		// add the previewText to the URL querystring    
		// have to URL encode it for it to work correctly
	    var params = "t=" + previewText;
	
		// do the fun ajax!    
	    retrieveURL(url, "PREVIEW", commentId, params);
	}

	function hideArea(theType) {
		var elementName = 'postcomment' + commentId;
		var previewName = 'previewcomment' + commentId;
	
		// always hide the error div		
		if (hasError) {
			var errorDiv = "errorcomment" + commentId;
			Element.hide(errorDiv);
			hasError = false;
		}
		
	    if (postingComment) {	
	        // we must hide the current stored commentId
	        // if we are canceling, we do it pretty
	        if (theType == "cancel") {
				Element.hide(previewName);        
		        Effect.SlideUp(elementName, { duration: 0.5 });
		    }
		    else {
		    	// otherwise we are posting another comment
		    	// so do it REAL quick
				Element.hide(previewName);	    	
				document.getElementById(elementName).innerHTML = "";
			}
	    }
	}

	function figureOut(theForm) {
// 		alert(buttonState);
		
		if (buttonState == 'preview') {
			previewComment(theForm);
		}
		else if (buttonState == 'cancel') {
			cancelPost();
		}
		else if (buttonState == 'post') {
			submitForm(theForm);
		}
	}

	// this checks the comments for when someone types, and if it's different than what's being preview we
	// turn off the post button
	function checkComment() {

		var tTitle = "";
		var tText = "";
		
		if (document.forms['rateForm'].elements['postCommentTitle']) {
			tTitle = document.forms['rateForm'].elements['postCommentTitle'].value;
		}
		
		if (document.forms['rateForm'].elements['postCommentTextArea']) {
			tText = document.forms['rateForm'].elements['postCommentTextArea'].value;
		}
	    
	   
 //    		alert(tTitle + " " + previewTitle + " : " + tText + " " + previewText);
	    if (canPost && (tText != previewText || tTitle != previewTitle)) {
	    	disallowPost();
	   	}
	}

	function disallowPost() {
	    canPost = false;
	    document.getElementById("postbutton" + commentId).disabled = true;
	}

	function allowPost() {
	    canPost = true;
	    var elem = document.getElementById("postbutton" + commentId);
	    
	    elem.disabled = false;
	    
	    // alert(previewingComment);
	    
	    if (!previewingComment) {
			window.setTimeout("document.getElementById('postbutton' + commentId).focus()", 501);
	    }
	    else {
		    try {
		    	 document.getElementById('postbutton' + commentId).focus();
		   	} catch (e) {
		   		alert(e);
		   	}
	    }
		
//		alert(elem);
//	    document.getElementById("postbutton" + commentId).focus();
	//    alert(document.getElementById("postbutton" + commentId).focus);
	}

  function retrieveURL(url, type, id, params) {
    which = id;
    if (window.XMLHttpRequest) { // Non-IE browsers
        req = new XMLHttpRequest();

      
        // pretty sure this adds a listener, which handles communication
        // listeners apparently don't like parameters, either
        if ("RATE" == type) {
            req.onreadystatechange = processRateStateChange;          
        }
        else if ("RECOMMEND" == type) {
            req.onreadystatechange = processRecommendStateChange;          
        } 
        else if ("PREVIEW" == type) {
			req.onreadystatechange = processPreviewComment;                  
        }      
        else {
            req.onreadystatechange = processStateChange;
        }

        try {
            req.open("POST", url, true);
	   		req.setRequestHeader("Content-type", "application/x-www-form-urlencoded; charset=UTF-8");
			req.setRequestHeader("Content-length", params.length);
			req.setRequestHeader("Connection", "close");            	    	
            req.send(params);
        } catch (e) {
            alert(e);
        }
    } 
    else if (window.ActiveXObject) { // IE
        req = new ActiveXObject("Microsoft.XMLHTTP");
		
        if (req) {
            if ("RATE" == type) {
                req.onreadystatechange = processRateStateChange;          
            }
            else if ("RECOMMEND" == type) {
                req.onreadystatechange = processRecommendStateChange;          
            }       
            else if ("PREVIEW" == type) {
				req.onreadystatechange = processPreviewComment;                  
	   		}   
            else {
                req.onreadystatechange = processStateChange;
            }
        
            req.open("POST", url, true);
	   		req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
			req.setRequestHeader("Content-length", params.length);
			//req.setRequestHeader("Connection", "close");            	    	
            req.send(params);
        }
    }
  }
  

  // listener
  function processStateChange() {
    
    if (req.readyState == 4) { // Complete
      if (req.status == 200) { // OK response
        document.getElementById("theTable").innerHTML = req.responseText;

      } else {

        alert("Problem: " + req.statusText);

      }

    }
  }
  
  
  
	function processPreviewComment() {

		if (req.readyState == 4) { // Complete
			if (req.status == 200) { // OK response
				var postCommentDiv = document.getElementById("postcomment" + commentId);
				var previewCommentDiv = document.getElementById("previewcomment" + commentId);
				
			  	// make it visible
			    // document.getElementById("previewComment").style.display = "block";
			    
				previewText = req.responseText;
				
				var commentError = false;
				
				if (previewText.indexOf("@@@ERROR@@@") != -1) {
					previewText = "<div class='soapbloxError'>" + previewText.substring(11, previewText.length) + "</div>";						
					commentError = true;
				}
    
		        // set values in the preview area
			    document.getElementById("previewTitle" + commentId).innerHTML = previewTitle;
			    document.getElementById("previewCommentText" + commentId).innerHTML = previewText;
			    
			    // alert(previewingComment); 
    
    			// if we're not already previewing a comment we slide down
    			if (!previewingComment) {
					Effect.SlideDown("previewcomment" + commentId, { duration: 0.5, scaleFrom: 3});
					//window.setTimeout("postButton.focus()", 501);
				}
				// otherwise, we just update the text
				else {
					Element.show("previewcomment" + commentId);
				}
			
				if (commentError) {
					disallowPost();
				}					
				else {
					allowPost();
				}
				
				previewingComment = true;
			}
			else {
				alert("Problem: " + req.statusText);
			}
		}	
	}
  
    function processRateStateChange() {
        //alert(loadName);
        //alert(document.getElementById(loadName));

        if (req.readyState == 4) { // Complete
        
            var loadName = "rateLoad" + which; 
            // alert("elem: " + document.getElementById(loadName));
            var elem = document.getElementById(loadName);
           

            if (req.status == 200) { // OK response
                elem.innerHTML = "<i>Complete!</i>";
                if (debug) {
                    alert(document.getElementById("rating" + which));
                    alert(req.responseText);
                }
        	   
                document.getElementById("rating" + which).innerHTML = req.responseText;
            } 
            else 
            {
                elem.innerHTML = "<b>Error!</b>";            
            }
            
            Effect.Fade(loadName);
        }
    } 
  
    function processRecommendStateChange() {
        // alert("elem: " + document.getElementById(loadName));
        var elem = document.getElementById("recommendLoad");
        var recommender = document.getElementById("recommender");
    
        if (req.readyState == 4) {
            if (req.status == 200) {
                elem.innerHTML = "<i>Complete!</i>";            
                recommender.innerHTML = req.responseText;                
            }
            else {
                elem.innerHTML = "<b>Error!</b>";  
            }
            
            recommender.disabled = false;
        }
        
        Effect.Fade("recommendLoad");        
    }
     

    function rate(theCommentId) {
        var selectName = "select" + theCommentId;
        var selector = document.rateForm.elements[selectName];
        var rateValue = selector.options[selector.selectedIndex].value;
        var url = "ajaxRate.do";
        var params = "id=" + theCommentId + "&value=" + rateValue;
        if (debug) {
            alert("commentId" + commentId + ", value: " + rateValue  + "\nurl: " + url);
        }
        
        var loadName = "rateLoad" + theCommentId;            
        // alert("loadName: " + loadName);
        var elem = document.getElementById(loadName);
        // alert("elem: " + elem);
        Element.show(elem);
        elem.innerHTML = "<I>Processing...</i>";
        
        retrieveURL(url, "RATE", theCommentId, params);
    }
    
    function recommend(diaryId) {

        var url = baseUrl + "ajaxRecommend.do";
        var params = "id=" + diaryId;
        
        if (debug) {
            alert("diaryId: " + diaryId);        
            alert("url: " + url);        
        }
        
        var recommender = document.getElementById("recommender");
        recommender.disabled = true;
        
        var elem = document.getElementById("recommendLoad");
        Element.show(elem);
        elem.innerHTML = "<br /><i>Processing...</i>";
        
        retrieveURL(url, "RECOMMEND", diaryId, params);        
    }
    
	function toggleEditor(id) {
		var elm = document.getElementById(id);

		if (tinyMCE.getInstanceById(id) == null)
			tinyMCE.execCommand('mceAddControl', false, id);
		else
			tinyMCE.execCommand('mceRemoveControl', false, id);
	} 
	
	function turnOnEditor(id) {
		if (tinyMCE.getInstanceById(id) == null)
			tinyMCE.execCommand('mceAddControl', false, id);	
	}
	
	function turnOffEditor(id) {
		if (id == "postCommentTextArea" && !tinyMCE.getInstanceById(id)) {
			id = "commentText";		
		}
		
		// alert(id);

		if (tinyMCE.getInstanceById(id) != null)
			tinyMCE.execCommand('mceRemoveControl', false, id);
	}	
	
	function setDiaryButtons(id) {
		var selectedValue = id.options[id.selectedIndex].value;	
		
		if (selectedValue == "WYSIWYG") { // which is WYSIWYG
			disableDiaryButtons(true);
		}
		else {		
			disableDiaryButtons(false);		
		}
	}
	
	
	function disableDiaryButtons(theValue) {
		document.getElementById("bold1").disabled = theValue;
		document.getElementById("italic1").disabled = theValue;
		document.getElementById("quote1").disabled = theValue;
		document.getElementById("bold2").disabled = theValue;
		document.getElementById("italic2").disabled = theValue;
		document.getElementById("quote2").disabled = theValue;	
	}	
	
	function setCommentButtons(id) {
		var selectedValue = id.options[id.selectedIndex].value;	
		
		if (selectedValue == "WYSIWYG") { // which is WYSIWYG
			disableCommentButtons(true);
		}
		else {		
			disableCommentButtons(false);		
		}
	}	
	
	function disableCommentButtons(theValue) {
		document.getElementById("bold1").disabled = theValue;
		document.getElementById("italic1").disabled = theValue;
		document.getElementById("quote1").disabled = theValue;
	}	

	
	function toggleDiaryEditor(id) {
		var selectedValue = id.options[id.selectedIndex].value;
		// alert(selectedValue);		
		
		setDiaryButtons(id);
		
		if (selectedValue == "WYSIWYG") { // which is WYSIWYG
			turnOnEditor("editMainText");
			turnOnEditor("editExtendedText");	
		}
		else {
			turnOffEditor("editMainText");
			turnOffEditor("editExtendedText");			

		}
	}   
	
	function toggleCommentEditor(id) {
		var selectedValue = id.options[id.selectedIndex].value;
		
		setCommentButtons(id);	
		
		// alert(selectedValue);
		
		if (selectedValue == "WYSIWYG") { // which is WYSIWYG
			turnOnEditor("postCommentTextArea");
		}
		else {
		    // alert("before turn off");
			turnOffEditor("postCommentTextArea");
		}
	} 
	
	function toggleQuickHitEditor(id) {
		var selectedValue = id.options[id.selectedIndex].value;
		
		if (selectedValue == "WYSIWYG") { // which is WYSIWYG
			turnOnEditor("quickHitText");
		}
		else {
			turnOffEditor("quickHitText");
		}
	} 		

	function addTags(textAreaId, startTag, endTag, content) {
		
		var textArea;
		
		if (document.forms['rateForm']) {
			textArea = document.forms['rateForm'].elements[textAreaId];
		}
		else {
			textArea = document.getElementById(textAreaId);
		}
		
		if (typeof textArea.selectionStart != "undefined") {
			textArea.focus();
			var selStart = textArea.selectionStart;
			var selEnd = textArea.selectionEnd;
			var selText = textArea.value.substring(selStart, selEnd);
			var scrlTop = textArea.scrollTop;
			var scrlDown = textArea.value.length <= selStart;
			if (selText.indexOf(startTag) === 0 && endTag == selText.substring(selText.length - endTag.length, selText.length)) {
				textArea.value = textArea.value.substring(0, selStart) + selText.substring(startTag.length, selText.length - endTag.length) 
					+ textArea.value.substring(selEnd, textArea.value.length);
				textArea.selectionStart = selStart;
				textArea.selectionEnd = selEnd - (startTag.length + endTag.length);
				textArea.scrollTop = scrlTop;
			} else {		
				textArea.value = textArea.value.substring(0, selStart) + startTag + (content ? content : selText)
					+ endTag + textArea.value.substring(selEnd, textArea.value.length);
				textArea.selectionStart = selStart;
				textArea.selectionEnd = selStart + startTag.length + (content ? content.length : selText.length) + endTag.length;
				textArea.scrollTop = (scrlDown ? 10000000 : scrlTop);
			}
		} else if (document.selection) {
			var range = document.selection.createRange();
			if (range.parentElement() != textArea) {
				var textRange = textArea.createTextRange();
				textRange.moveStart('character', textArea.value.length);
				textRange.collapse();
				textRange.select();
				range = document.selection.createRange();
			}
			textArea.focus();
			if (range.text.indexOf(startTag) === 0 && endTag == range.text.substring(range.text.length - endTag.length, range.text.length)) {
				range.text = range.text.substring(startTag.length, range.text.length - endTag.length);
			} else {
				range.text = startTag + (content ? content : range.text) + endTag;
			}
		} else {
			textArea.focus();
			textArea.value = startTag + (content ? content : "") + endTag;
			if (typeof textArea.scrollTop != "undefined") {
				textArea.scrollTop = 10000000;
			}
		}
	}
	
	
	function addBoldTags(textAreaId) {
		addTags(textAreaId, "<b>", "</b>");
		return false;
	}
	
	function addItalicTags(textAreaId) {
		addTags(textAreaId, "<i>", "</i>");
		return false;
	}
	
	function addQuoteTags(textAreaId) {
		addTags(textAreaId, "<blockquote>", "</blockquote>");
		return false;
	}
		