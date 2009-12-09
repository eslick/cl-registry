function surveyEditCreateSortable(widget_id)
{
    var list_id = widget_id + '-sortable';
    var code = $("reorder-action-code").value;

    Sortable.create(list_id, {
	tag: 'div', only: ['group-summary-widget'],
	constraint: false,
	delay: 500,	
	scroll: window,
        onUpdate: function(list) {
            var result = Sortable.serialize(list);
	    initiateActionWithArgs(code, "", { list: result }, "post");
        }
    });
}

function surveyEditCreateDroppable(widget_id)
{
    Droppables.add(widget_id, {
	accept: 'scratchpad-group',
	hoverclass: 'dropAllowed',
	onDrop: function(thing) {
//		    window.console.log("received " + thing.id);

		    var code = $('accept-drop-action-code').value;
		    initiateActionWithArgs(code,"",{ item: thing.id }, "post");
		}
    });
}

/*
 * This function is called whenever the survey widget or
 * any of its constituent group-summary-widgets is
 * re-rendered.  (See the render-widget-body methods for
 * those widgets.)  It would be nice if there was a better way.
 */
function surveyEditInitialize() {
    var widgets = $$('.survey-editor-widget');

    widgets.each(function (s) {
        var sid = s.getAttribute('id');
        surveyEditCreateSortable(sid);
        surveyEditCreateDroppable(sid);
    });
}

Event.onReady(function() { surveyEditInitialize(); });

/* Handle selection */

function surveyEditSetSelection(id) {
    var code = $("update-selection-action-code").value;
    initiateActionWithArgs(code, "", { id: id }, "post");
}

Event.addBehavior({
    'div.group-summary-widget:click' : function(e) {
	var elt = e.element();
	/* This may be inadequate, but the idea is that we don't want to
	 * interfere with clicks on certain elements.
         */
	if (elt.tagName == 'A') {
	    return;
        }

	surveyEditSetSelection(this.id);

	var id = this.id;
	var groups = $$('.group-summary-widget');

	groups.each(function(gw) {
	    if (gw.id.toString() == id.toString()) {
		gw.addClassName('selected');
	    } else {
		gw.removeClassName('selected');
	    }
	});
    }
});
