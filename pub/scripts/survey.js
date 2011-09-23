
//
// Support for dynamic inline groups
//

Event.onReady(function() {
    window.scrollTo(0,0);
});

Event.addBehavior.reassignAfterAjax = true;

Event.addBehavior({
    'div.inline-trigger input[type=checkbox]:click' : function (e) {
	var element = Event.element(e);
        element.value = element.checked ? 't' : 'nil';
        element.checked = true;
	updateAnswersOnChange(element.identify());
    },
//    'div.inline-trigger input:change' : function (e) {
//	var element = Event.element(e);
//	if (!element.match('input.radio') || !element.match('input[type=checkbox]')) {
//	    updateAnswersOnChange (element.identify());
//	}
//    },
// NEW
    'div.inline-trigger input[type=radio]:change' : function (e) {
	var element = Event.element(e);
	if (!element.match('input[type=radio]') && 
            !element.match('input[type=checkbox]')) {
	    updateAnswersOnChange (element.identify());
	}
    },
    'div.inline-trigger input[type=radio]:click' : function (e) {
	updateAnswersOnChange (Event.element(e).identify());
    },
    'label.radio input:click' : function (e) {
	updateAnswersOnChange (Event.element(e).identify());
    },
    'div.inline-trigger select:change' : function (e) {
	updateAnswersOnChange (Event.element(e).identify());
    },
    'div.inline-trigger textarea:change' : function (e) {
	updateAnswersOnChange (Event.element(e).identify()); 
    }
});


var inFlightID = 0;

function updateAnswersOnChange (id) {
    var action = Form.getInputs('survey-form', 'hidden', 'action');
    var action_string = action[0].getAttribute('value');

    inFlightID = inFlightID + 1;
    customInitiateFormAction(action_string, $('survey-form'), "p", id);
//    customInitiateFormAction(action_string, $('survey-form'), "p", document.activeElement.identify());
}

function customInitiateFormAction(actionCode, form, sessionString, id) {
    // A grody hack, but it works...
    var serializedForm = form.serialize(true);
    delete(serializedForm['action']);
    delete(serializedForm['continue']);
    serializedForm['update'] = 'Update';

    new Ajax.Request(getActionUrl(actionCode, sessionString),
		     {
			 method: form.method,
			 onSuccess: function (transport) {
			     inFlightID = inFlightID - 1;
			     if(inFlightID <= 0) {
				 var focus_id = document.activeElement.identify();
				 onActionSuccess(transport);
				 $(focus_id).focus(); 
			     }
//			     focusNextElement(id);
			 },
			 onFailure: function (transport) {
			     inFlightID = inFlightID - 1;
			     onActionFailure(transport);
			 },
			 parameters: serializedForm
		     });
}

function focusNextElement(id) {
    var prior_element = $(id);
    var form_elements =  Form.getElements('survey-form');
    var prior_found = false;
    var next = form_elements.find(function (e) {
	if (prior_found) return true; 
	else if (e == prior_element) { 
	    prior_found = true; 
	    return false; 
	}
	else return false;
    });
    if (next != false) {
	next.focus();
    } else {
	prior_element.focus();
    }
}


//Event.addBehavior({
//    '#update:click' : function (e) {
//	alert('update clicked');
//    }});