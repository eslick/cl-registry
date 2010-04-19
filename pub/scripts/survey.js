
//
// Support for dynamic inline groups
//

Event.onReady(function() {
    window.scrollTo(0,0);
});


Event.addBehavior.reassignAfterAjax = true;

Event.addBehavior({
    'div.inline-trigger input:change' : function (e) {
	var element = Event.element(e);
	if (!element.match('input.radio')) {
	    updateAnswersOnChange (element.identify());
	}
    },
    'div.inline-trigger input[type=checkbox]:click' : function (e) {
        Event.element(e).value = Event.element(e).checked ? 't' : 'nil';
        Event.element(e).checked = true;
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


function updateAnswersOnChange (id) {
    var action = Form.getInputs('survey-form', 'hidden', 'action');
    var action_string = action[0].getAttribute('value');
				
    customInitiateFormAction(action_string, $('survey-form'), "p", id);
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
			     onActionSuccess(transport);
			     focusNextElement(id);
			 },
			 onFailure: onActionFailure,
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