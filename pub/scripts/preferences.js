// mainly just copied from survey.js

Event.addBehavior.reassignAfterAjax = true;

Event.addBehavior({
    'div#preferences-body input:change' : function (e) {
	updateAnswersOnChange (Event.element(e).identify());
    },
    'div#preferences-body select:change' : function (e) {
        window.console.log("select change");
	updateAnswersOnChange (Event.element(e).identify());
    },
    'div#preferences-body textarea:change' : function (e) {
	updateAnswersOnChange (Event.element(e).identify()); 
    }
});


function updateAnswersOnChange (id) {
    var action = Form.getInputs('preferences-form', 'hidden', 'action');
    var action_string = action[0].getAttribute('value');

//    window.console.log("updating " + id);
				
    customInitiateFormAction(action_string, $('preferences-form'), "p", id);
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
			     //focusNextElement(id);
			 },
			 onFailure: onActionFailure,
			 parameters: serializedForm
		     });
}

function focusNextElement(id) {
    var prior_element = $(id);
    var form_elements =  Form.getElements('preferences-form');
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

