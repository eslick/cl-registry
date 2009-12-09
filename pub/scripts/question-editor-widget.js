function questionEditInitialize() {
    var widgets = $$('.question-editor-widget');

    widgets.each(function(w) {
	var wid = w.getAttribute('id');
	/*
 	 * Eat up these events so that the
	 * HTML controls don't activate.
	 */
	Event.observe(wid, 'mouseup', function(e) {
	    e.stop();
	});
	Event.observe(wid, 'mousedown', function(e) {
	    e.stop();
	});
    });
}

/*
Event.onReady(function() { questionEditInitialize(); });
*/

function updateAnswerType(dtype, vtype) {
	var code = $('answer-type-action-code').value;
	initiateActionWithArgs(code, "", { dtype: dtype, vtype: vtype },
				 "post");
}

/*
Event.addBehavior({
    '#answer-type-text-line:click' : function(e) {
	window.console.log("clicked on " + this);
	e.stop();
	updateAnswerType("string", "text-field");
    },
    '#answer-type-text-paragraph:click' : function(e) {
	window.console.log("clicked on " + this);
	e.stop();
	updateAnswerType("string", "paragraph");
    },
    '#answer-type-number:click' : function(e) {
	window.console.log("clicked on " + this);
	e.stop();
	updateAnswerType("number", "text-field");
    },
    '#answer-type-date:click' : function(e) {
	window.console.log("clicked on " + this);
	e.stop();
	updateAnswerType("date", "text-field");
    },
    '#answer-type-checkbox:click' : function(e) {
	window.console.log("clicked on " + this);
	e.stop();
	updateAnswerType("boolean", "checkbox");
    },
    '#answer-type-dropdown:click' : function(e) {
	window.console.log("clicked on " + this);
	e.stop();
	updateAnswerType("choice", "dropdown");
    },
    '#answer-type-radio:click' : function(e) {
	window.console.log("clicked on " + this);
	e.stop();
	updateAnswerType("boolean", "radio");
    },
    '#answer-type-selection:click' : function(e) {
	window.console.log("clicked on " + this);
	e.stop();
	updateAnswerType("multichoice", "multiple-dropdown");
    }
});
*/ 
