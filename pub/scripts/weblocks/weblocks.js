
// Utilities
function updateElementBody(element, newBody) {
    element.update(newBody);
}

function updateElement(element, newElement) {
    element.replace(newElement);
}

function selectionEmpty() {
    if(document.getSelection) {
	return document.getSelection() == "";
    } else if(document.selection && document.selection.createRange) {
	return document.selection.createRange().text == "";
    } else {
	return true;
    }
}

function addCss(cssCode) {
    var styleElement = document.createElement("style");
    styleElement.type = "text/css";
    if (styleElement.styleSheet) {
	styleElement.styleSheet.cssText = cssCode;
    } else {
	styleElement.appendChild(document.createTextNode(cssCode));
    }
    document.getElementsByTagName("head")[0].appendChild(styleElement);
}

function stopPropagation(event) {
    if(event.preventDefault) {
	event.stopPropagation();
    } else {
	event.cancelBubble = true;
    };
}

// Register global AJAX handlers to show progress
Ajax.Responders.register({
  onCreate: function() {
	    $('ajax-progress').innerHTML = "<img src='/pub/images/ajax-loader.gif'>";
	}, 
  onComplete: function() {
	    $('ajax-progress').innerHTML = "";
	}
});

function onActionSuccess(transport) {
    // Grab json value
    var json;
    if(Prototype.Browser.WebKit) {
	// We should sanitize JSON, but at the moment it crashes Safari
        json = transport.responseText.evalJSON();
    } else {
        json = transport.responseText.evalJSON(true);
    }
    
    // See if there are redirects
    var redirect = json['redirect'];
    if (redirect)
    {
	window.location.href = redirect;
	return;
    }
    
    execJsonCalls(json['before-load']);

    // Update dirty widgets
    var dirtyWidgets = json['widgets'];
    for(var i in dirtyWidgets) {
	var widget = $(i);
	if(widget) {
            //console.log("updating widget %s", i);
	    updateElement(widget, dirtyWidgets[i]);
	}
    }

    execJsonCalls(json['on-load']);
}

function execJsonCalls (calls) {
    if(calls) {
	calls.each(function(item)
			 {
			     try {
                                 item.evalScripts();
			     } catch(e) {
//                                 console.log("Error evaluating AJAX script %o: %s", item, e);
                             }
			 });
    }
}

function onActionFailure() {
    alert('We could not complete your request because of an internal error.  Please e-mail LAMsightHelp@lamtreatmentalliance.org with the page you were on and what you were doing when you received this error and we will look into it for you.');
}

function getActionUrl(actionCode, sessionString, isPure) {
    var url = location.href.sub(/\?.*/, "") + '?' + sessionString + '&action=' + actionCode;
    if(isPure) {
	url += '&pure=true';
    }
    return url;
}

function initiateActionWithArgs(actionCode, sessionString, args, method, url) {
    if (!method) method = 'get';
    if (!url) url = getActionUrl(actionCode, sessionString);
    new Ajax.Request(url,
                     {
                         method: method,
                         onSuccess: onActionSuccess,
                         onFailure: onActionFailure,
                         parameters: args
                     });

}

/* convenience/compatibility function */
function initiateAction(actionCode, sessionString) {
    initiateActionWithArgs(actionCode, sessionString);
}

function initiateFormAction(actionCode, form, sessionString) {
    // Hidden "action" field should not be serialized on AJAX
    var serializedForm = form.serialize(true);
    delete(serializedForm['action']);

    initiateActionWithArgs(actionCode, sessionString, serializedForm, form.method);
} 

function disableIrrelevantButtons(currentButton) {
    $(currentButton.form).getInputs('submit').each(function(obj)
						   {
						       obj.disable();
						       currentButton.enable();
						   });
}

// Fix IE6 flickering issue
if(Prototype.Browser.IE) {
    try {
	document.execCommand("BackgroundImageCache", false, true);
    } catch(err) {}
}

// Table hovering for IE (can't use CSS expressions because
// Event.observe isn't available there and we can't overwrite events
// using assignment
if(!window.XMLHttpRequest) {
    // IE6 only
    Event.observe(window, 'load', function() {
	    var tableRows = $$('.table table tbody tr');
	    tableRows.each(function(row) {
		    Event.observe(row, 'mouseover', function() {
			    row.addClassName('hover');
			}); 
		    Event.observe(row, 'mouseout', function() {
			    row.removeClassName('hover');
			}); 
		});
	});
}

// Support suggest control
function declareSuggest(inputId, choicesId, resultSet, sessionString) {
    if(resultSet instanceof Array) {
	new Autocompleter.Local(inputId, choicesId, resultSet, {});
    } else {
	new Ajax.Autocompleter(inputId, choicesId, getActionUrl(resultSet, sessionString, true), {});
    }
}

function replaceDropdownWithSuggest(ignoreWelcomeMsg, inputId, inputName, choicesId, value) {
    var dropdownOptions = $(inputId).childElements();
    var suggestOptions = [];
    dropdownOptions.each(function(i)
			 {
			     if(!(i == dropdownOptions[0] && ignoreWelcomeMsg)) {
				 suggestOptions.push(i.innerHTML);
			     }
			 });

    var inputBox = '<input type="text" id="' + inputId + '" name="' + inputName + '" class="suggest"';
    if(value) {
	inputBox += 'value="' + value +'"';
    }
    inputBox += '/>';
    
    var suggestHTML = inputBox + '<div id="' + choicesId + '" class="suggest"></div>';
    $(inputId).replace(suggestHTML);
    
    declareSuggest(inputId, choicesId, suggestOptions);
}

function include_css(css_file) {
  var html_doc = document.getElementsByTagName('head').item(0);
  var css = document.createElement('link');
  css.setAttribute('rel', 'stylesheet');
  css.setAttribute('type', 'text/css');
  css.setAttribute('href', css_file);
  html_doc.appendChild(css);
  return false;
}

function include_dom(script_filename) {
  var html_doc = document.getElementsByTagName('head').item(0);
  var js = document.createElement('script');
  js.setAttribute('language', 'javascript');
  js.setAttribute('type', 'text/javascript');
  js.setAttribute('src', script_filename);
  html_doc.appendChild(js);
  return false;
}


//////////////////////////////////////////////////////////////
// Psuedo-http streaming interaction with server

function loopAsynchRequests () {
    setTimeout(performAsynchAPIRequest);
}

function performAsynchAPIRequest() {
    new Ajax.Request("/lsapi/?action=api-call",
		     {
			 onSuccess: onAsynchAPISuccess,
			 onFailure: onAsynchAPIFailure
		     });
}

function onAsynchAPISuccess(transport) {
    // Grab json value
    var json;
    if(Prototype.Browser.WebKit) {
	// We should sanitize JSON, but at the moment it crashes Safari
        json = transport.responseText.evalJSON();
    } else {
        json = transport.responseText.evalJSON(true);
    }
    
//    execJsonCallsNoCatch(json['on-load']);
    execJsonCalls(json['on-load']);

    setTimeout(performAsynchAPIRequest, 1000);
}

function execJsonCallsNoCatch(calls) {
    if(calls) {
	calls.each(function (item)
		   {
//		       try {
		       item.evalScripts();
//		       } 
//		   } catch(e) {
//		       console.log("Error evaluating AJAX script %o: %s", item, e);
//		   }
		   });
    }
}

function onAsynchAPIFailure(transport) {
    setTimeout(performAsynchAPIRequest, 5000);
}

////////////////////////////////////////////////////
// FLOTR, etc
//

function testDrawBar () {
    var elt = $('flotr')
    var ctx = elt.down('canvas').getContext('2d');
    ctx.lineWidth = 2;
    ctx.save();
    ctx.translate(100, 100);
    ctx.beginPath();
    ctx.lineTo(140, 140);
    ctx.stroke();
    ctx.strokeStyle = "#FF00FF";
    ctx.strokeRect(0, 0, 100, 100);
    ctx.restore();
}

//
// The idea here is to have a series of primitives predicatd on ID
//    that can draw elements on the canvas
//

function getContext (id) {
    return $(id).down('canvas').getContext('2d');
}

function lsDrawRect (id, x, y, width, height, options) {
    var ctx = getContext(id);
    ctx.save();
    ctx.strokeRect(x, y, width, height);
    ctx.restore();
}

function lsClear (id) {
    var ctx = getContext(id);
    ctx.save();
    ctx.clearRect(0, 0, $(id).getWidth(), $(id).getHeight());
    ctx.restore();
}